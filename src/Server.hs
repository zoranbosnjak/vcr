------------------
-- |
-- Module: Server
--
-- This module provides server access definitions.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

-- standard imports
import           Control.Exception (SomeException, try)
import           Control.Monad.IO.Class (liftIO)
import           GHC.Generics (Generic)
import           Data.Monoid ((<>))
import           Data.String
import qualified Options.Applicative as Opt
import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (fromStrict)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)

-- local imports
import           Common
import           Streams

newtype URI = URI String deriving (Generic, Eq, Show)
instance ToJSON URI
instance FromJSON URI

instance IsString URI where
    fromString = URI

type RetryTimeout = Double

-- | Server connection pool.
data ServerConnection = ServerConnection
    { serverURI       :: URI
    , retryTimeout    :: RetryTimeout
    } deriving (Generic, Eq, Show)

instance ToJSON ServerConnection
instance FromJSON ServerConnection

serverConnectionOptions :: Opt.Parser ServerConnection
serverConnectionOptions = ServerConnection
    <$> serverOptions
    <*> retryTimeoutOptions

serverOptions :: Opt.Parser URI
serverOptions = URI
    <$> Opt.strOption
        (Opt.long "uri"
       <> Opt.metavar "URI"
       <> Opt.help "URI address"
        )

retryTimeoutOptions :: Opt.Parser RetryTimeout
retryTimeoutOptions = Opt.option Opt.auto
    ( Opt.long "retry"
   <> Opt.metavar "SECONDS"
   <> Opt.help "Retry timeout"
   <> Opt.value 10
   <> Opt.showDefault
    )

serverWriter :: ServerConnection -> Consumer BS.ByteString
serverWriter sc = mkConsumer action where
    action consume = do
        manager <- liftIO $ newManager defaultManagerSettings
        initialRequest <- liftIO $ parseRequest $ uri ++ "events"
        forever $ do
            msg <- consume Clear
            let request = initialRequest
                    { method = "PUT"
                    , requestBody = RequestBodyLBS $ fromStrict msg
                    }
                sendToServer = do
                    rv <- try $ httpLbs request manager
                    let err = case rv of
                            Left (_e :: SomeException) ->
                                Just $ "server connection error " ++ show uri
                            Right resp ->
                                let rc = statusCode $ responseStatus resp
                                in case rc of
                                    200 -> Nothing
                                    _ -> Just $
                                        "unexpected response code from server "
                                        ++ show uri
                                        ++ ": " ++ show rc
                    case err of
                        Nothing -> do
                            logM DEBUG "data sent to server"
                        Just s -> do
                            logM NOTICE s
                            threadDelaySec rt
                            sendToServer
            liftIO $ sendToServer
    URI uri = serverURI sc
    rt = retryTimeout sc

