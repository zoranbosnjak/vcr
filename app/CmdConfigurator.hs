
-- | VCR recorder configurator GUI

module CmdConfigurator where

-- standard imports
import           Data.Aeson.Decoding
import           Data.ByteString     as BS
import           GHC.Generics        (Generic)
import           Options.Applicative

-- local imports
import           Common
import           Configurator

data CmdOptions = CmdOptions
    { optConfigFile :: Maybe FilePath
    , optRecorders  :: [(Name, String)]
    } deriving (Generic, Eq, Show)

options :: Parser CmdOptions
options = CmdOptions
    <$> optional (strOption (long "config" <> help "Load config file"))
    <*> some recorder
  where
    recorder :: Parser (Name, String)
    recorder = (,)
        <$> strOption (long "recorder" <> metavar "NAME" <> help "recorder name")
        <*> strArgument (metavar "URL" <> help "recorder URL")

runCmd :: CmdOptions -> Prog -> Args -> Version -> IO ()
runCmd opt _pName _pArgs _version = do
    initialConfig <- case optConfigFile opt of
        Nothing       -> pure mempty
        Just filename -> do
            s <- BS.readFile filename
            either fail pure $ Data.Aeson.Decoding.eitherDecodeStrict s
    runConfigurator (optRecorders opt) initialConfig

-- | toplevel command
cmdConfigurator :: ParserInfo Command
cmdConfigurator = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Recorder configurator")
