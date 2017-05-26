------------------
-- |
-- Module: CmdArchive
--
-- 'archive' command
--

module CmdArchive (cmdArchive) where

-- standard imports
import Control.Exception as CE
import qualified Data.ByteString as BS
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

import Test.QuickCheck hiding (output)

import System.IO
import Data.ByteString.Char8 (pack)

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
-- (2) if there is some data left at the end of a file that cannot be
--     recognized as an event.
copyFromFileToFile :: File.FileStore -> File.FileStore -> IO ()
copyFromFileToFile inpFS outFS = do
    logM INFO $ "archive: file to file"
    -- Open both files (or throw an exception in case of an error).
    inpH <- CE.catch (openFile (File.filePath inpFS) ReadMode)
                     ((\ _->C.throw ("Cannot open input file '" ++
                                     (File.filePath inpFS) ++ "'."))
                      ::SomeException -> IO a)
    outH <- CE.catch (openFile (File.filePath outFS) WriteMode)
                     ((\ _->C.throw ("Cannot open output file '" ++
                                     (File.filePath outFS) ++ "'."))
                      ::SomeException -> IO a)
    -- Copy events from one handle to another.
    rest <- copy BS.empty inpH outH
    -- Close the files.
    hClose outH
    hClose inpH
    -- Check whether thare is some data left at the end of a file that
    -- cannot be recognized as an event.
    if (BS.null rest) then return () else
        C.throw ("Unrecognized data at the end of file '" ++
                 (File.filePath inpFS) ++ "'.")
  where
    -- Copies the events stored in one file (specified as a handle
    -- opened for reading) to another file (specified as a handle for
    -- writing) and returns the unused content of the end of the input
    -- file, i.e., content that cannot be recognized as an event.  The
    -- first argument acts as a buffer.
    copy :: BS.ByteString -> Handle -> Handle -> IO BS.ByteString
    copy buffer inpH outH = do
        case (Encodings.decodeFirst (File.fileEnc inpFS) buffer)
             :: (Maybe Event.Event, BS.ByteString) of
          (Nothing,oldBuffer) -> do
              -- No event can be taken out of the buffer: read the
              -- next chunk of data and continue unless no more data
              -- can be read.
              addBuffer <- BS.hGet inpH 1024
              logM DEBUG $ "archive: read from file"
              let newBuffer = BS.append oldBuffer addBuffer
              if BS.null addBuffer then return oldBuffer else
                  copy newBuffer inpH outH
          (Just event,remBuffer) -> do
              -- An event has been taken out of the buffer: write a
              -- single event and the delimiter out and continue
              -- copying.
              BS.hPut outH (Encodings.encode (File.fileEnc outFS) event)
              BS.hPut outH (pack (Encodings.delimit (File.fileEnc outFS)))
              logM DEBUG $ "archive: wrote to file"
              copy remBuffer inpH outH



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

