--------
-- Common functions and definitions
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
    ( Prog, Args, Version, Command, GhcBase
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
    , Alarm(..), newAlarm, runAlarm, refreshAlarm, getAlarm
    , setupLogging
    )
  where

import           Control.Monad
import           Control.Concurrent.STM
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Data.Bool
import           Data.Maybe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16

import qualified System.Log.Logger as Log
import           System.Log.Logger (Priority(..))
import           System.Log.Handler.Simple (verboseStreamHandler)
import           System.Log.Handler.Syslog (openlog, Option(PID), Facility(USER))
import           System.IO


type Prog = String
type Args = [String]
type Version = String
type GhcBase = String
type Command = Prog -> Args -> Version -> GhcBase -> IO ()

type LoggerName = String
type ErrorMsg = String

-- | Alarm with automatic timeout.
data Alarm a = Alarm
    { almTimeout :: Double      -- timeout in seconds
    , almQueue :: TQueue a      -- message queue
    , almAlm :: TVar (Maybe a)  -- current alarm value
    }

-- | Create alarm
newAlarm :: Double -> IO (Alarm a)
newAlarm t = Alarm
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
refreshAlarm :: Alarm a -> a -> IO ()
refreshAlarm alm val = atomically $ writeTQueue (almQueue alm) val

-- | Get current alarm value.
getAlarm :: Alarm a -> IO (Maybe a)
getAlarm = atomically . readTVar . almAlm

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
restartOnChange :: Eq a => STM a -> (a -> IO b) -> IO b
restartOnChange getVar act = atomically getVar >>= go where
    go x = do
        result <- race (act x) $ atomically $ do
            y <- getVar
            bool (return y) retry (y == x)
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
    -> IO (LoggerName -> Priority -> String -> IO ())
setupLogging pName cmdName optVerbose optSyslog = do

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
          where
            fullName = pName ++ "/" ++ cmdName ++ "/" ++  name
            -- Make sure that a message is not too long (problems with syslog).
            n = 1000
            msg = case Prelude.length s > n of
                True -> Prelude.take n s ++ "..."
                False -> s

    return logM

