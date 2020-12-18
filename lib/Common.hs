--------
-- Common functions and definitions
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
    ( Prog, Args, Version, Command, GhcBase, WxcLib
    , LoggerName
    , ErrorMsg
    , Priority(..)
    , threadDelaySec
    , runAll
    , periodic
    , runMaybe
    , doNothing
    , whenSpecified
    , restartOnChange
    , hexlify, unhexlify
    , Alarm(..), newAlarmIO, runAlarm, refreshAlarm, getAlarm
    , UpdatingVar(..), newUpdatingVarIO, updateVar, restartOnUpdate
    , setupLogging
    , encodeCompact, encodeJSON, decodeJSON
    , newline
    )
  where

import           Control.Monad
import           Control.Concurrent.STM
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Data.Bool
import           Data.Maybe
import           Data.Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import           Data.Word (Word8)

import qualified System.Log.Logger as Log
import           System.Log.Logger (Priority(..))
import           System.Log.Handler.Simple (verboseStreamHandler)
import           System.Log.Handler.Syslog (openlog, Option(PID), Facility(USER))
import           System.IO

type Prog = String
type Args = [String]
type Version = String
type GhcBase = String
type WxcLib = String
type Command = Prog -> Args -> Version -> GhcBase -> WxcLib -> IO ()

type LoggerName = String
type ErrorMsg = String

data UpdatingVar a = UpdatingVar a (TQueue a)

-- | Create UpdatingVar
newUpdatingVarIO :: a ->  IO (UpdatingVar a)
newUpdatingVarIO val = UpdatingVar <$> pure val <*> newTQueueIO

-- | Update 'UpdatingVar'
updateVar :: UpdatingVar a -> a -> STM ()
updateVar (UpdatingVar _initial queue) val = writeTQueue queue val

-- | Restart process on variable update.
restartOnUpdate :: UpdatingVar a -> (TVar a -> IO b) -> IO b
restartOnUpdate (UpdatingVar initial queue) act = go initial where
    go x = do
        var <- newTVarIO x
        race (act var) (atomically $ readTQueue queue) >>= \case
            Left a -> return a
            Right y -> go y

-- | Alarm with automatic timeout.
data Alarm a = Alarm
    { almTimeout :: Double      -- timeout in seconds
    , almQueue :: TQueue a      -- message queue
    , almAlm :: TVar (Maybe a)  -- current alarm value
    }

-- | Create alarm
newAlarmIO :: Double -> IO (Alarm a)
newAlarmIO t = Alarm
    <$> pure t
    <*> newTQueueIO
    <*> newTVarIO Nothing

-- | Run alarm inside IO.
runAlarm :: Alarm a -> IO ()
runAlarm (Alarm t q alm) = waitMessage where
    waitMessage = do
        atomically (readTQueue q >>= writeTVar alm . Just)
        expire <- registerDelay $ round $ t * 1000 * 1000
        waitMessageOrExpire expire
    waitMessageOrExpire expire = do
        let expired = readTVar expire >>= bool retry (writeTVar alm Nothing)
            msgReady = isEmptyTQueue q >>= bool (return ()) retry
        atomically $ expired `orElse` msgReady
        waitMessage

-- | Refresh alarm with new message.
refreshAlarm :: Alarm a -> a -> STM ()
refreshAlarm alm val = writeTQueue (almQueue alm) val

-- | Get current alarm value.
getAlarm :: Alarm a -> STM (Maybe a)
getAlarm = readTVar . almAlm

-- | Wait for given number of seconds.
threadDelaySec :: Double -> IO ()
threadDelaySec = threadDelay . round . (1000000*)

-- | Run action periodically (with delay in between).
periodic :: Double -> IO a -> IO b
periodic period act = forever (act >> threadDelaySec period)

-- | Sleep indefinetly.
doNothing :: IO a
doNothing = periodic 1.0 $ return ()

-- | Run all processes concurrently, return index of a terminated process.
runAll :: [IO ()] -> IO Int
runAll lst = go $ zip [0..] lst where
    go [] = doNothing >> return (-1)
    go ((ix,p):[]) = p >> return ix
    go ((ix,p):rest) = race p (go rest) >>= \case
        Left _a -> return ix
        Right b -> return b

-- | Run action if given argument is (Just x).
runMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
runMaybe mVal act = maybe (return ()) act mVal

-- | Run process if given argument is (Just x), otherwise run empty loop.
whenSpecified :: (t -> IO a) -> Maybe t -> IO a
whenSpecified _act Nothing = doNothing
whenSpecified act (Just val) = act val

-- | Restart process on STM value change.
restartOnChange :: Eq c => STM a -> (a -> c) -> (a -> IO b) -> IO b
restartOnChange getVar compareValue act = atomically getVar >>= go where
    go x = do
        result <- race (act x) $ atomically $ do
            y <- getVar
            bool (return y) retry (compareValue y == compareValue x)
        case result of
            Left a -> return a
            Right y -> go y

-- | Convert bytestring to hex representation.
hexlify :: BS.ByteString -> String
hexlify = BS8.unpack . B16.encode

-- | Convert hex representation back to a bytestring.
unhexlify :: String -> Maybe BS.ByteString
unhexlify st = do
    let (a,b) = B16.decode $ BS8.pack st
    guard $ BS.null b
    return a

-- | Setup logging, return logM function.
setupLogging :: Prog -> String -> Maybe Priority -> Maybe Priority
    -> Maybe (Priority -> String -> String -> IO ())
    -> IO (LoggerName -> Priority -> String -> IO ())
setupLogging pName cmdName optVerbose optSyslog optAux = do

    -- setup logging
    when (isJust optVerbose || isJust optSyslog) $ do
        Log.updateGlobalLogger Log.rootLoggerName
            (Log.setLevel minBound . Log.removeHandler)

        -- console logger
        runMaybe optVerbose $ \level -> do
            hConsole <- verboseStreamHandler stdout level
            Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler hConsole)

        -- syslog
        runMaybe optSyslog $ \level -> do
            sl <- openlog (pName) [PID] USER level
            Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler sl)

    let logM :: LoggerName -> Priority -> String -> IO ()
        logM name prio s = do
            Log.logM fullName prio msg
            case optAux of
                Nothing -> return ()
                Just func -> func prio cmdName s
          where
            fullName = pName ++ "/" ++ cmdName ++ "/" ++  name
            -- Make sure that a message is not too long (problems with syslog).
            n = 1000
            msg = case Prelude.length s > n of
                True -> Prelude.take n s ++ "..."
                False -> s

    return logM

-- | Encode to JSON.
encodeCompact :: ToJSON a => a -> BSL.ByteString
encodeCompact = Data.Aeson.encode

-- | Encode JSON object.
encodeJSON :: ToJSON a => a -> BS.ByteString
encodeJSON = BSL.toStrict . encodeCompact

-- | Decode JSON object, fail on error.
decodeJSON :: FromJSON a => BS.ByteString -> a
decodeJSON s = case decodeStrict s of
    Nothing -> error $ "Can not decode line: " ++ show s
    Just val -> val

-- | Newline character, represented as Word8 (ByteString)
newline :: Word8
newline = BS.head $ BS8.singleton '\n'

