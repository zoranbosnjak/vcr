{- |
Description : Recorder configuration example.

This module is an example recorder configurator.

Usage:

To run a program:
vcr custom --program "</abs/path/to/this/script> --script-arguments" --run

To check:
vcr custom --program "</abs/path/to/this/script>" --validate
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

newtype Options = Options
    { optConfigFile :: Maybe FilePath
    } deriving (Show)

options :: Parser Options
options = Options
    <$> optional (strOption (long "config" <> help "Load config file"))

main :: IO ()
main = do
    opt <- execParser (info (options <**> helper) idm)
    initialConfig <- case optConfigFile opt of
        Nothing        -> pure mempty
        Just _filename -> undefined
    runConfigurator recorders initialConfig
