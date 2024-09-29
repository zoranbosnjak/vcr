-- | VCR logging framework.

module Logging
    ( module Logging
    , Priority (..)
    )
    where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           System.IO
import           System.Log.Handler.Simple (verboseStreamHandler)
import           System.Log.Handler.Syslog (Facility (USER), Option (PID),
                                            openlog)
import qualified System.Log.Logger         as Log
import           System.Log.Logger         (Priority (..))

type ProgName = String
type LoggerName = String

type Logger m = LoggerName -> Priority -> String -> m ()

-- | Ignore log messages.
noLogger :: Monad m => Logger m
noLogger _loggerName _prio _s = pure ()

-- | Format log message.
fmtLog :: LoggerName -> Priority -> String -> String
fmtLog loggerName prio s = loggerName ++ " " ++ show prio ++ ": " ++ s

-- | Log to stderr.
stderrLogger :: MonadIO m => Logger m
stderrLogger loggerName prio s = liftIO $ hPutStrLn stderr $
    fmtLog loggerName prio s

-- | Log to a file.
fileLogger :: MonadIO m => FilePath -> Logger m
fileLogger path loggerName prio s = liftIO . withFile path AppendMode $ \h -> do
    hPutStrLn h $ fmtLog loggerName prio s

-- | Setup logging, based on given options, return logM function.
setupLogging :: ProgName -> LoggerName -> Maybe Priority -> Maybe Priority -> Maybe (Logger IO) -> IO (Logger IO)
setupLogging pName cmdName optVerbose optSyslog optAux = do
    when (isJust optVerbose || isJust optSyslog) $ do
        Log.updateGlobalLogger Log.rootLoggerName
            (Log.setLevel minBound . Log.removeHandler)

        -- console logger
        case optVerbose of
            Nothing -> pure ()
            Just level -> do
                hConsole <- verboseStreamHandler stdout level
                Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler hConsole)

        -- syslog
        case optSyslog of
            Nothing -> pure ()
            Just level -> do
                sl <- openlog pName [PID] USER level
                Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler sl)

    let logM :: LoggerName -> Priority -> String -> IO ()
        logM name prio s = do
            Log.logM fullName prio msg
            case optAux of
                Nothing   -> pure ()
                Just func -> func cmdName prio s
          where
            fullName = pName ++ "/" ++ cmdName ++ "/" ++  name
            -- Make sure that a message is not too long.
            -- 'syslog' has problems with long messages.
            n = 1000
            msg = case Prelude.length s > n of
                True  -> Prelude.take n s ++ "..."
                False -> s

    pure logM
