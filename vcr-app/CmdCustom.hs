
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module CmdCustom where

-- standard imports
import           GHC.Generics (Generic)
import           Options.Applicative
import           System.Exit (ExitCode(ExitFailure,ExitSuccess))
import           System.Process (runProcess, waitForProcess)
import           System.IO.Temp (withSystemTempDirectory)
import           System.FilePath ((</>))
import           System.Posix.Process (executeFile)

-- local imports
import           Common

data CmdOptions = CmdOptions
    { optProgram :: FilePath
    , optGhcOpts :: Maybe String
    , optCompileOnly :: Bool
    } deriving (Generic, Eq, Show)

options :: Parser CmdOptions
options = CmdOptions
    <$> strOption (long "program" <> metavar "CMD"
        <> help "custom program file with optional arguments")
    <*> optional (strOption (long "ghcOpts" <> metavar "OPTS"
        <> help "arguments to ghc"))
    <*> switch (long "compile-only"
        <> help "stop after compile stage, do not run the program")

-- Recompile configuration file and execute it.
runCmd :: CmdOptions -> Prog -> Args -> Version -> GhcBase -> WxcLib -> IO ()
runCmd opt _pName _pArgs _version ghcBase wxcLib = do
    withSystemTempDirectory "vcr" $ \tmp -> do
        let target = tmp <> "vcr"
            commandLine = words $ optProgram opt
            cmd = head commandLine
            cmdArgs = tail commandLine
        ghcProc <- runProcess (ghcBase </> "bin" </> "ghc")
            (
            [ "--make"
            , "-O2"
            , "-Wall"
            , "-threaded"
            , "-v0"
            , "-outputdir", tmp
            , "-L"++wxcLib
            , "-o", target
            , cmd
            ]
            ++ words (maybe "" id (optGhcOpts opt))
            )
            (Just tmp)
            Nothing
            Nothing
            Nothing
            Nothing
        waitForProcess ghcProc >>= \case
            ExitFailure _rc -> return ()
            ExitSuccess -> case optCompileOnly opt of
                True -> return ()
                False -> executeFile target False
                    cmdArgs
                    Nothing     -- env

cmdCustom :: ParserInfo Command
cmdCustom = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Run custom program")

