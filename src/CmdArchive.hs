module CmdArchive (cmdArchive) where

import Options.Applicative
import Data.Monoid

import Action

{-

TODO: At the moment, the dbase is specified using a simple string.
Once the dbase is implemented, this should change to allow proper
dbase specification.

-}

cmdArchive :: ParserInfo (Action ())
cmdArchive =
    info (helper <*> (runCmd <$> optionParser))
         (progDesc "Event archiver")

optionParser :: Parser Options
optionParser =
    (pure Options)
    <*> (subparser (command "input"
           (info (helper <*> inputParser)
                 (progDesc "Input definition"))))
    <*> (subparser (command "output"
           (info (helper <*> outputParser)
                 (progDesc "Onput definition"))))

data Options
    = Options
      { optInput :: Input
      , optOutput :: Output
      }

data Input
    = InputFile String
    | InputDBase String

inputParser :: Parser Input
inputParser =
    (subparser.foldl1 (<>))
    [command "file"
       (info (helper <*> (InputFile
          <$> argument str (metavar "FILE" <> help "Input file name")))
       (progDesc "Input file specification"))
    ,command "dbase"
       (info (helper <*> (InputDBase
          <$> argument str (metavar "DBASE" <> help "Input dbase name")))
       (progDesc "Input dbase specification"))]

data Output
    = OutputFile String
    | OutputDBase String

outputParser :: Parser Output
outputParser =
    (subparser.foldl1 (<>))
    [command "file"
       (info (helper <*> (OutputFile
          <$> argument str (metavar "FILE" <> help "Output file name")))
       (progDesc "Output file specification"))
    ,command "dbase"
       (info (helper <*> (OutputDBase
          <$> argument str (metavar "DBASE" <> help "Output dbase name")))
       (progDesc "Output dbase specification"))]

runCmd :: Options -> Action ()
runCmd opts = do
    nop

{- EOF -}
