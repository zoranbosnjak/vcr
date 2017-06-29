------------------
-- |
-- Module: CmdArchive
--
-- TODO: WRITE COMPREHENSIVE DOCUMENTATION OF THE ARCHIVER


module CmdArchive (cmdArchive) where

-- Standard imports.
import qualified Control.Exception    as CE
import           Control.Monad           (forM_)
import qualified Data.ByteString.Lazy as BSL
import           Options.Applicative     ((<**>), (<|>))
import qualified Options.Applicative  as Opt
import           System.IO
import           System.Log.Logger       (Priority(INFO, DEBUG, NOTICE))
-- import Control.Exception as CE
-- import Data.Time (UTCTime(UTCTime))
-- import Network.HTTP.Client.Conduit

-- Local imports.
import qualified Common as C
import qualified Event
import qualified Server as Srv
import qualified File
import qualified Encodings
-- import Test.QuickCheck hiding (output)



-- | The exported function implementing the entire functionality of
-- the archiver.  For more information see the documentation at the
-- top of this source file.
cmdArchive :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdArchive = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event archiver")



-- | Archiver specific command options.
data Options = Options
    { optInput          :: Input
    , optOutput         :: Output
    -- TODO: , optChannelFilter  :: [Event.Channel]
    -- TODO: , optSourceIdFilter :: [Event.SourceId]
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
    -- <*> Opt.many Event.channelOptions
    -- <*> Opt.many Event.sourceIdOptions
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
    C.logM INFO $
        "archive: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    C.logM NOTICE $
        "archive: no server support implemented yet"

    case (optInput opts,optOutput opts) of
      (IFile inpFS,OFile outFS) ->
          copyFromFileToFile inpFS outFS
      _ ->
          C.throw "TODO"

    C.logM INFO $
        "archive: done"



-- | Copies one file of events to another file of events, possibly in
-- a different format.
-- Throws an exception
-- (1) if any IO operation on files fails, or
-- (2) if any segment of the input file cannot be recognized as an event.
copyFromFileToFile :: File.FileStore -> File.FileStore -> IO ()
copyFromFileToFile inpFS outFS = do
    C.logM DEBUG $ "archive: file to file started"

    -- Open the input and the output file:
    inpFH <- if (File.filePath inpFS)=="-" then return stdin else
                 CE.catch (openFile (File.filePath inpFS) ReadMode)
                   ((\ _ -> do
                         C.throw ("archive: Cannot open input file '"++
                                  (File.filePath inpFS)++"'."))
                    :: (CE.IOException -> IO Handle))
    outFH <- if (File.filePath outFS)=="-" then return stdout else
                 CE.catch (openFile ((File.filePath outFS)++"") WriteMode)
                   ((\ _ -> do
                         hClose inpFH
                         C.throw ("archive: Cannot open output file '"++
                                  (File.filePath outFS)++"'."))
                    :: (CE.IOException -> IO Handle))

    -- Input and output encodings:
    let inpEnc = File.fileEnc inpFS
    let outEnc = File.fileEnc outFS

    -- Get the complete input string and split it to bytestrings
    -- representing individual events using the separator defined by
    -- the input encoding:
    bss <- Encodings.split inpEnc <$> BSL.hGetContents inpFH

    -- Decode each bytestring representing an individual event write
    -- the event to the output file one by one.
    -- TODO: check performance, take more events at the same time.
    forM_ (take 100000 bss) $ \ bs -> case Encodings.decode inpEnc bs of
        Nothing -> do
            hClose outFH
            hClose inpFH
            C.throw "archive: Cannot decode an event."
        Just event -> do
            C.logM DEBUG $ "archive: event " ++ show (Event.eUtcTime event)
            BSL.hPutStr outFH
              (Encodings.encode outEnc (event::Event.Event))

    -- Close the input and the output file:
    if (File.filePath inpFS)=="-" then return () else hClose outFH
    if (File.filePath outFS)=="-" then return () else hClose inpFH
    
    C.logM DEBUG $ "archive: file to file done"
    return ()
