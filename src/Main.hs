-------
-- | Event recorder/replay,... main module
--

module Main where

-- standard imports
import           Control.Exception (catch, SomeException)
import           Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import           Control.Monad (when)
import           Data.Maybe (isJust)
import           System.Log.Logger (Priority(INFO))
import qualified System.Log.Logger as Log
import           System.Log.Handler.Simple (verboseStreamHandler)
import           System.Log.Handler.Syslog
                    (openlog, Option(PID), Facility(USER))
import           System.IO (stdout, stderr, hPutStrLn)
import           System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import qualified System.Environment
import           System.Remote.Monitoring

-- local imports
import qualified Common as C
import           CmdRecord    (cmdRecord)
import           CmdReplay    (cmdReplay)
import           CmdArchive   (cmdArchive)
import           CmdServe     (cmdServe)

-- | Available commands.
commands :: [(String, Opt.ParserInfo (C.VcrOptions -> IO ()))]
commands =
    [ ("record",    cmdRecord)
    , ("replay",    cmdReplay)
    , ("archive",   cmdArchive)
    , ("serve",     cmdServe)
    ]

-- | Toplevel command line options.
data Options = Options
    { optGlobal :: C.VcrOptions
    , optCommand :: C.VcrOptions -> IO ()
    }

-- | Toplevel command line parser.
options :: Opt.Parser Options
options = Options
    <$> C.vcrOptions
    <*> Opt.subparser (mconcat [Opt.command a b | (a,b) <- commands])

main :: IO ()
main = do

    -- parse options
    opt <- Opt.execParser (Opt.info (options <**> Opt.helper) Opt.idm)

    pName <- System.Environment.getProgName
    pArgs <- System.Environment.getArgs

    -- EKG monitor
    runMaybe (C.vcrOptEkg $ optGlobal opt) $ \(ip, port) -> do
        _ <- forkServer ip port
        return ()

    -- setup logging
    when (isJust (C.vcrOptVerbose (optGlobal opt)) ||
          isJust (C.vcrOptSyslog (optGlobal opt))) $ do

        Log.updateGlobalLogger Log.rootLoggerName
            (Log.setLevel minBound . Log.removeHandler)

        -- console logger
        runMaybe (C.vcrOptVerbose $ optGlobal opt) $ \level -> do
            hConsole <- verboseStreamHandler stdout level
            Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler hConsole)

        -- syslog
        runMaybe (C.vcrOptSyslog $ optGlobal opt) $ \level -> do
            sl <- openlog (pName) [PID] USER level
            Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler sl)

    -- run command
    C.logM INFO $ "startup " ++ show pName ++ " " ++ show pArgs
    (optCommand opt $ optGlobal opt) `catch` onError
    exitWith ExitSuccess

  where

    onError :: SomeException -> IO ()
    onError e = do
        hPutStrLn stderr "VCR error:"
        hPutStrLn stderr $ show e
        exitWith $ ExitFailure 1

    runMaybe mVal act = maybe (return ()) act mVal

