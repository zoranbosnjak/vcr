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
import           Data.Foldable (fold)
import qualified Options.Applicative as Opt
import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (fromStrict)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import qualified Data.Sequence as DS
import           Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async

-- local imports
import           Common
import           Streams
import qualified Buffer
import qualified Encodings as Enc

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

serverWriter :: ServerConnection -> Buffer.Threshold
    -> (BS.ByteString -> IO ()) -> Consumer BS.ByteString ()
serverWriter sc th dropAct = mkConsumer action where
    URI uri = serverURI sc
    rt = retryTimeout sc

    sender rxBuf txBuf = do
        manager <- newManager defaultManagerSettings
        initialRequest <- parseRequest $ uri ++ "events"
        forever $ do

            content <- atomically $ do
                msgs <- readTVar rxBuf
                case DS.null msgs of
                    True -> retry
                    False -> do
                        writeTVar txBuf msgs
                        writeTVar rxBuf DS.empty
                        return $ fold msgs

            let request = initialRequest
                    { method = "PUT"
                    , requestBody = RequestBodyLBS $ fromStrict content
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
            sendToServer
            atomically $ writeTVar txBuf DS.empty

    acquire = do
        rxBuf <- newTVarIO DS.empty
        txBuf <- newTVarIO DS.empty
        a <- Async.async $ sender rxBuf txBuf
        Async.link a
        return (rxBuf, txBuf, a)

    release (rxBuf, txBuf, a) = do
        Async.cancel a
        content <- atomically $ do
            c1 <- readTVar txBuf
            c2 <- readTVar rxBuf
            return $ fold $ c1 DS.>< c2
        dropAct content

    action consume = bracket acquire release $ \(rxBuf,txBuf,_) -> forever $ do
        msg <- consume Clear
        dropContent <- liftIO $ atomically $ do
            msgs <- (DS.|> msg) <$> readTVar rxBuf
            let check1 = maybe False (DS.length msgs >) (Buffer.thLength th)
                bytes = sum $ fmap Enc.sizeOf msgs
                check2 = maybe False (bytes >) (Buffer.thBytes th)
            case (check1 || check2) of
                True -> do
                    writeTVar rxBuf DS.empty
                    c1 <- readTVar txBuf
                    writeTVar txBuf DS.empty
                    return $ fold $ c1 DS.>< msgs
                False -> do
                    writeTVar rxBuf msgs
                    return BS.empty
        liftIO $ dropAct dropContent

