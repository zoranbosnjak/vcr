{- |
Description : Recorder configuration example.

This module is an example recorder configurator.

Usage:

# to run a program.
vcr custom --program "</abs/path/to/this/script> --script-arguments" --run

# to check
vcr custom --program "</abs/path/to/this/script>" --validate

# to make it executable, use shebang, something like this
#! /usr/bin/env -S vcr-custom.sh --script-arguments
-}

{-# LANGUAGE OverloadedStrings #-}

-- standard imports
--
-- VCR imports
import           Configurator

-- List of recorders.
recorders :: [(Name, String)]
recorders =
    [ ("rec1", "http://127.0.0.1:12345/")
    , ("rec2", "https://127.0.0.1:12346/")
    -- add more recorders as necessary
    ]

data Options = Options
    { optConfigFile :: Maybe FilePath
    } deriving (Show)

options :: Parser Options
options = Options
    <$> optional (strOption (long "config" <> help "Load config file"))

main :: IO ()
main = do
    opt <- execParser (info (options <**> helper) idm)
    initialConfig <- case optConfigFile opt of
        Nothing -> return mempty
        Just _filename -> undefined
    runConfigurator recorders initialConfig

