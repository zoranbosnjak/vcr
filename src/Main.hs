-------
-- | Event recorder/replay,... main module
--

module Main where

-- standard imports
import Control.Monad.Trans.Except
import Options.Applicative
import System.IO
import System.Exit
import System.Log.Logger (Priority(..))
import System.Log.Logger hiding (logM)
import System.Log.Handler.Simple

-- local imports
import Action
import CmdRecord
import CmdServe

data Options = Options
    { optDebug :: Maybe Priority
    , optAction :: Action ()
    }

commands :: [(String, ParserInfo (Action ()))]
commands =
    [ ("recorder", cmdRecord)
    , ("server", cmdServe)
    ]

options :: Parser Options
options = Options
    <$> optional (option auto (long "debug" <> help "enable debugging"))
    <*> subparser ( mconcat [command a b | (a,b) <- commands])

main :: IO ()
main = do
    opt <- execParser (info (helper <*> options) idm)

    -- update logger
    case optDebug opt of
        Nothing -> return ()
        Just level -> do
            updateGlobalLogger rootLoggerName (setLevel DEBUG . removeHandler)
            hConsole <- verboseStreamHandler stdout level
            updateGlobalLogger rootLoggerName (addHandler hConsole)

    -- run command
    res <- runExceptT $ optAction opt
    case res of
        Left (Error e) -> do
            hPutStrLn stderr "error(s):"
            hPutStrLn stderr e
            exitWith $ ExitFailure 1
        Right _ -> do
            exitWith ExitSuccess

