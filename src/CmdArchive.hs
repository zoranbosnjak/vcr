------------------
-- |
-- Module: CmdArchive
--
-- 'archive' command
--

module CmdArchive (cmdArchive) where

-- standard imports
import Control.Monad (forM_)
--import Control.Exception as CE
import qualified Data.ByteString.Lazy as BSL
--import Data.Time (UTCTime(UTCTime))
import Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import System.Log.Logger (Priority(INFO, DEBUG, NOTICE))

-- local imports
--import qualified Buffer
import Common (logM)
import qualified Common as C
import qualified Event
import qualified Server as Srv
import qualified File
import qualified Encodings

-- import Test.QuickCheck hiding (output)

import System.IO

cmdArchive :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdArchive = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event archiver")

-- | Speciffic command options.
data Options = Options
    { optInput          :: Input
    , optOutput         :: Output
    , optChannelFilter  :: [Event.Channel]
    , optSourceIdFilter :: [Event.SourceId]
    -- TODO: , optStartTime      :: Maybe UTCTime
    -- TODO: , optStopTime       :: Maybe UTCTime
    -- TODO: , read/write batch size and speed
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IFile File.FileStore
    | IServer Srv.ServerConnection
    deriving (Eq, Show)

-- | Output options.
data Output
    = OFile File.FileStore
    | OServer Srv.ServerConnection
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> input
    <*> output
    <*> Opt.many Event.channelOptions
    <*> Opt.many Event.sourceIdOptions
    -- <*> Opt.optional (C.timeOptions "start")
    -- <*> Opt.optional (C.timeOptions "stop")
    -- some more options...

input :: Opt.Parser Input
input = C.subparserCmd "input ..." $ Opt.command "input" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data source")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *> (IFile <$> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server")
        *> (IServer <$> Srv.serverConnectionOptions)

output :: Opt.Parser Output
output = C.subparserCmd "output ..." $ Opt.command "output" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data destination")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *> (OFile <$> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server")
        *> (OServer <$> Srv.serverConnectionOptions)

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "archive, opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    logM NOTICE $
        "archive: no server support implemented yet"

    -- TODO: to be finalized when server support is added
    inpFS <- case (optInput opts) of
        IFile fileStore -> return fileStore
        IServer _       -> error "INPUT: No server support yet."
    outFS <- case (optOutput opts) of
        OFile fileStore -> return fileStore
        OServer _       -> error "OUTPUT: No server support yet."

    copyFromFileToFile inpFS outFS

-- | Copies one file of events to another file of events, possibly in
-- a different format.
-- Throws an exception
-- (1) if either file cannot be opened;
-- (2) if any segment cannot be recognized as an event.
copyFromFileToFile :: File.FileStore -> File.FileStore -> IO ()
copyFromFileToFile inpFS outFS = do
    logM INFO $ "archive: file to file"

    -- open input file, auto close
    withFile (File.filePath inpFS) ReadMode $ \inputH -> do
        let inputEnc = File.fileEnc inpFS

        -- get complete input string,
        -- split to individual bytestrings (lazy)
        -- use separator as defined by the input encoding
        lst <- Encodings.split inputEnc <$> BSL.hGetContents inputH

        -- For each bytestring from the list,
        -- try to decode as event, then append to output file.
        -- For the moment, append events to an output file one by one,
        -- TODO: check performance, take more events at the time...
        forM_ lst $ \i -> case Encodings.decode inputEnc i of
            Nothing -> C.throw "unable to decode event"
            Just event -> do
                logM DEBUG $ "got event, " ++ show (Event.eUtcTime event)
                File.appendFile outFS [event]

-------------------------------------------------------------------------------
{-

    -- generating test input
    events <- sample' (arbitrary::Gen Event.Event)
    File.appendFile (File.FileStore "input.text" Encodings.EncText) events
    File.appendFile (File.FileStore "input.bin" Encodings.EncBin) events
    File.appendFile (File.FileStore "input.json" (Encodings.EncJSON (Encodings.JSONCompact))) events
    File.appendFile (File.FileStore "input.json4" (Encodings.EncJSON (Encodings.JSONPretty 4))) events

    -- simple copy from one file to another, no error recovery
    inpBS <- BS.readFile (File.filePath inpFileStore)
    events <- return ((Encodings.decodeList (File.fileEnc inpFileStore) inpBS)
                      ::(Maybe [Event.Event]))
    File.appendFile outFileStore
      (case events of
         Just events -> events
         Nothing     -> [])

-}

