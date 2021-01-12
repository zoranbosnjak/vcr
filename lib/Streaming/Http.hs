{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Streaming.Http where

import           GHC.Generics (Generic)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except
import qualified Data.Vector as V
import           Data.Aeson
import           Data.Void
import qualified Data.Aeson.Types as AT
import qualified System.IO as IO
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Time
import           Data.List (sort)
import           Data.Maybe
import           System.Directory
import           System.FilePath
import qualified Data.Text as T
import           Text.Printf
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as L
import           Test.QuickCheck

import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception (try)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Client as HTC
import qualified Network.HTTP.Client.TLS as HTCS
import qualified Network.HTTP.Types as HT

import           Pipes
import           Pipes.Safe
import qualified Pipes.Prelude as PP

-- local imports
import           Time
import           Vcr
import           Common

import           Streaming.Disk (DirectoryIndex)

-- | Pre-fetch buffer size.
prefetch :: Int
prefetch = 1000

-- | Url encode JSON object
objToUrl :: ToJSON a => a -> String
objToUrl = BS8.unpack . HT.urlEncode False . encodeJSON

-- | Create manager helper function.
mkManager :: HTC.Request -> IO HTC.Manager
mkManager request = HTC.newManager $ case HTC.secure request of
    False -> HTC.defaultManagerSettings
    True -> HTCS.tlsManagerSettings

-- | Make http(s) GET request and perform some action on response.
runGetRequest :: String -> (HTC.Response HTC.BodyReader -> IO a) -> IO a
runGetRequest url act = do
    request <- HTC.setRequestCheckStatus <$> HTC.parseRequest url
    manager <- mkManager request
    HTC.withResponse request manager act

-- | Fetch bytest via GET.
fetchUrlRaw :: String -> IO BS.ByteString
fetchUrlRaw url = runGetRequest url consumer where
    consumer resp = accumulate mempty where
        accumulate x = HTC.responseBody resp >>= \s -> case BS.null s of
            True -> return x
            False -> accumulate (x <> s)

-- | Fetch JSON object via GET.
fetchUrl :: FromJSON a => String -> IO a
fetchUrl url = fetchUrlRaw url >>= decodeJSON

-- | Create 'player' from request.
playUrl :: String -> Producer BS8.ByteString (SafeT IO) ()
playUrl url = bracket acquire HTC.responseClose $ \h ->
    fix $ \loop -> do
        chunk <- liftIO $ HTC.responseBody h
        case BS.null chunk of
            True -> return ()   -- no more data
            False -> yield chunk >> loop
  where
    acquire = do
        request <- HTC.setRequestCheckStatus <$> HTC.parseRequest url
        manager <- mkManager request
        HTC.responseOpen request manager

-- | Split bytestring to lines.
toLines :: Functor m => BS.ByteString -> Pipe BS.ByteString BS.ByteString m c
toLines accumulator = do
    chunk <- await
    remaining <- process (accumulator <> chunk)
    toLines remaining
  where
    process buffer = case BS.elemIndex newline buffer of
        Nothing -> return buffer
        Just ix -> do
            let (a,b) = BS.splitAt ix buffer
            yield a
            process $ BS.drop 1 b -- drop newline

-- | Decode bytestring to events.
decodeEvents :: (MonadThrow m, FromJSON a) => Pipe BS.ByteString a m r
decodeEvents = forever $ do
    s <- await
    lift (decodeJSON s) >>= yield

-- | Http/Https streaming

data HttpArchive = HttpArchive String

mkHttpPlayer :: FromJSON a => HttpArchive -> Player (SafeT IO) Index (Event a)
mkHttpPlayer (HttpArchive url) = Player
    { limits = liftIO $ fetchUrl
        ( url ++ "limits"
        )
    , middle = \ix1 ix2 -> liftIO $ fetchUrl
        ( url ++ "middle"
       ++ "?ix1=" ++ objToUrl ix1
       ++ "&ix2=" ++ objToUrl ix2
        )
    , peekItem = \ix -> liftIO $ do
        s <- fetchUrlRaw
            ( url ++ "peek"
           ++ "?ix=" ++ objToUrl ix
            )
        decodeJSON s

    , mkPlayer = \direction ix flt ->
        let request = url ++ "events"
                ++ "?ix=" ++ objToUrl ix
                ++ "&includeIndex"
                ++ case direction of
                    Forward -> ""
                    Backward -> "&backward"
                ++ case flt of
                    Nothing -> ""
                    Just (val1, timeout) ->
                        "&ch=" ++ objToUrl val1
                        ++ case timeout of
                            Nothing -> ""
                            Just val2 -> "&timeout=" ++ objToUrl val2
        in playUrl request >-> toLines mempty >-> decodeEvents
    }

