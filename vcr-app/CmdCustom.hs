
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
import           System.Directory (copyFile)
import           System.Posix.Process (executeFile)

-- local imports
import           Common

data CustomAction
    = Validate
    | Build FilePath
    | Run
    deriving (Generic, Eq, Show)

data CmdOptions = CmdOptions
    { optProgram :: FilePath
    , optGhcOpts :: Maybe String
    , optAction :: CustomAction
    } deriving (Generic, Eq, Show)

options :: Parser CmdOptions
options = CmdOptions
    <$> strOption (long "program" <> metavar "CMD"
        <> help "custom program file with optional arguments")
    <*> optional (strOption (long "ghcOpts" <> metavar "OPTS"
        <> help "arguments to ghc"))
    <*> (validate <|> build <|> run)
  where
    validate = flag' Validate ( long "validate"
       <> help "stop after compile stage, do not run the program")
    build = Build <$> strOption ( long "build"
        <> metavar "FILENAME"
        <> help "save compiled filed to a target binary file, do not run the program" )
    run = flag' Run ( long "run" <> help "compile and run the prorgam, remove compiled binary on exit")

-- Recompile configuration file and execute it.
runCmd :: CmdOptions -> Prog -> Args -> Version -> GhcBase -> WxcLib -> IO ()
runCmd opt _pName _pArgs _version ghcBase wxcLib = do
    withSystemTempDirectory "vcr" $ \tmp -> do
        let buildTarget = tmp <> "vcr"
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
            , "-o", buildTarget
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
            ExitSuccess -> case optAction opt of
                Validate -> return ()
                Build dst -> copyFile buildTarget dst
                Run -> executeFile buildTarget False
                    cmdArgs
                    Nothing     -- env

cmdCustom :: ParserInfo Command
cmdCustom = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Run custom program")

