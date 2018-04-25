-------
-- | Event recorder/replay,... main module
--

module Main where

-- standard imports
import           Control.Exception (catch, SomeException)
import           Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(INFO))
import qualified System.Log.Logger as Log
import           System.Log.Handler.Simple (verboseStreamHandler)
import           System.Log.Handler.Syslog
                    (openlog, Option(PID), Facility(USER))
import           System.IO (stdout, stderr, hPutStrLn)
import           System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import qualified System.Environment

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

    -- console logger
    case C.vcrOptVerbose (optGlobal opt) of
        Nothing -> return ()
        Just level -> do
            Log.updateGlobalLogger Log.rootLoggerName
                (Log.setLevel minBound . Log.removeHandler)
            hConsole <- verboseStreamHandler stdout level
            Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler hConsole)

    -- syslog
    case C.vcrOptSyslog (optGlobal opt) of
        Nothing -> return ()
        Just level -> do
            Log.updateGlobalLogger Log.rootLoggerName
                (Log.setLevel minBound . Log.removeHandler)
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

