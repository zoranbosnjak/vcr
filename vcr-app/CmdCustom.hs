
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
    } deriving (Generic, Eq, Show)

options :: Parser CmdOptions
options = CmdOptions
    <$> strOption (long "program" <> metavar "FILE" <> help "custom program file")
    <*> optional (strOption (long "ghcOpts" <> metavar "OPTS" <> help "arguments to ghc"))

runCmd :: CmdOptions -> Prog -> Args -> Version -> GhcBase -> IO ()
runCmd opt _pName _pArgs _version ghcBase = do
    -- recompile configuration file and execute it
    withSystemTempDirectory "vcr" $ \tmp -> do
        let target = tmp <> "vcr"
        ghcProc <- runProcess (ghcBase </> "bin" </> "ghc")
            (
            [ "--make"
            , "-v0"
            , "-outputdir", tmp
            , "-o", target
            , optProgram opt
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
            ExitSuccess -> executeFile target False
                []          -- args
                Nothing     -- env

cmdCustom :: ParserInfo Command
cmdCustom = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Run custom program")

