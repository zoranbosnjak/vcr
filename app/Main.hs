{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Main VCR application.

module Main where

-- standard imports
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import           Data.Functor
import           Options.Applicative
import qualified System.Environment
import           System.Exit              (exitSuccess)
import qualified System.Remote.Monitoring as Ekg
import           Text.Read                (readMaybe)

-- local imports
import           CmdCapture               (cmdCapture)
import           CmdCat                   (cmdCat)
import           CmdConfigurator          (cmdConfigurator)
import           CmdServer                (cmdServer)
import           CmdTestGenerator         (cmdTestGenerator)
import           CmdTestReceiver          (cmdTestReceiver)
import           Common
import           TH                       (getEnvVariableExpr)

commands :: [(String, ParserInfo Command)]
commands =
    [ ("capture", cmdCapture)
    , ("configurator", cmdConfigurator)
    , ("server", cmdServer)
    , ("cat", cmdCat)
    , ("test-generator", cmdTestGenerator)
    , ("test-receiver", cmdTestReceiver)
    ]

data Options = Options
    { optCommand :: Command
    , optEkg     :: Maybe (BS.ByteString, Int)
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

    -- parse options
    opt <- do
        let showVersion = flag' True (long "version" <> help "Show version and exit")
            options'
                = (showVersion $> Nothing)
              <|> fmap Just options
        execParser (info (options' <**> helper) idm) >>= \case
            Nothing -> do
                putStrLn $ pName ++ ", " ++ versionString
                exitSuccess
            Just opt -> pure opt

    -- EKG monitor
    runMaybe (optEkg opt) $ \(ip, port) -> do
        _ <- Ekg.forkServer ip port
        pure ()

    optCommand opt pName pArgs versionString
