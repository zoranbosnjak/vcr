
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- standard imports
import           Options.Applicative
import           Control.Exception (catch, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified System.Environment
import           Text.Read (readMaybe)
import           System.IO (stderr, hPutStrLn)
import           System.Exit (exitWith, ExitCode(ExitFailure,ExitSuccess))
import qualified System.Remote.Monitoring as Ekg

-- local imports
import           Common
import           TH (getEnvVariableExpr)
import           CmdCustom (cmdCustom)
import           CmdCapture (cmdCapture)
import           CmdServer (cmdServer)

commands :: [(String, ParserInfo Command)]
commands =
    [ ("custom", cmdCustom)
    , ("capture", cmdCapture)
    , ("server", cmdServer)
    ]

data Options = Options
    { optCommand :: Command
    , optEkg        :: Maybe (BS.ByteString, Int)
    }

options :: Parser Options
options = Options
    <$> subparser (mconcat [command a b | (a,b) <- commands])
    <*> optional (option ekgOption
        ( long "ekg"
       <> metavar "IP:PORT"
       <> help "Enable EKG monitor"
        ))
  where
    ekgOption = maybeReader $ \s -> do
        let (a,b) = break (==':') s
            ip = BS8.pack a
        port <- readMaybe $ drop 1 b
        Just (ip, port)

main :: IO ()
main = do
    pName <- System.Environment.getProgName
    pArgs <- System.Environment.getArgs

    -- get build environment variables
    let
        swVersion :: String
        swVersion = $( getEnvVariableExpr "SW_VERSION" )

        gitRev :: String
        gitRev = $( getEnvVariableExpr "GIT_REV" )

        versionString :: String
        versionString =
            "version: " ++ swVersion
            ++ ", git rev: " ++ gitRev

        ghcBase :: String
        ghcBase = $( getEnvVariableExpr "GHC_BASE" )

        wxcLib :: String
        wxcLib = $( getEnvVariableExpr "WXC_LIB" )

    -- parse options
    opt <- do
        let showVersion = flag' True (long "version" <> help "Show version and exit")
            options'
                = (showVersion *> pure Nothing)
              <|> fmap Just options
        execParser (info (options' <**> helper) idm) >>= \case
            Nothing -> do
                putStrLn $ pName ++ ", " ++ versionString
                exitWith ExitSuccess
            Just opt -> return opt

    -- EKG monitor
    runMaybe (optEkg opt) $ \(ip, port) -> do
        _ <- Ekg.forkServer ip port
        return ()

    let onError :: SomeException -> IO ()
        onError e = do
            hPutStrLn stderr $ show pName ++ " error:"
            hPutStrLn stderr $ show e
            exitWith $ ExitFailure 1

    (optCommand opt) pName pArgs versionString ghcBase wxcLib `catch` onError

