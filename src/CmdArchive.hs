------------------
-- |
-- Module: CmdArchive
--
-- Archiver is used for transfering recorded data events from a file
-- or server to another file or server.
--
-- (*) The transfer of recorded data is carried out in chunks.  The
--     size of a chunk is specified by a command-line option
--     'chunkSize' (default=1024).
-- (*) Events are encoded as specified in 'Encodings'.  The maximal
--     size of a recorded event is specified by a comman-line option
--     'maxEventSize' (default=16384).
--
-- TODO: WRITE COMPREHENSIVE DOCUMENTATION OF THE ARCHIVER

module CmdArchive (cmdArchive) where

-- Standard imports.
import           Control.Concurrent (threadDelay)
import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import           Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(INFO, DEBUG, NOTICE))

-- Local imports.
import qualified Common as C
import qualified Event
import qualified Server
import qualified File
import qualified Encodings
import           Streams



-- | The exported function implementing the entire functionality of
-- the archiver.  For more information see the description at the top.
cmdArchive :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdArchive = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event archiver")



-- | Archiver specific command options.
data Options = Options
    { optInput          :: Input
    , optOutput         :: Output
    , optChunkSize      :: Int
    , optMaxEventSize   :: Int
    , optChunkDelay     :: Int
    , optEventDelay     :: Int
    , optChannelFilter  :: [Event.Channel]
    , optSourceIdFilter :: [Event.SourceId]
    , optStartTime      :: Maybe Event.UtcTime
    , optEndTime        :: Maybe Event.UtcTime
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IFile Encodings.EncodeFormat File.FileStore
    | IServer Server.ServerConnection
    deriving (Eq, Show)

-- | Output options.
data Output
    = OFile Encodings.EncodeFormat File.FileStore
    | OServer Server.ServerConnection
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> input
    <*> output
    <*> Opt.option Opt.auto
        ( Opt.long "chunkSize"
       <> Opt.help ("the chunk size")
       <> Opt.showDefault
       <> Opt.value 1024
       <> Opt.metavar "CHUNKSIZE")
    <*> Opt.option Opt.auto
        ( Opt.long "maxEventSize"
       <> Opt.help ("the maximal event size")
       <> Opt.showDefault
       <> Opt.value 16384
       <> Opt.metavar "MAXEVENTSIZE")
    <*> Opt.option Opt.auto
        ( Opt.long "chunkDelay"
       <> Opt.help ("Time delay (in ms) between processing chunks.")
       <> Opt.showDefault
       <> Opt.value 0
       <> Opt.metavar "CHUNKDELAY")
    <*> Opt.option Opt.auto
        ( Opt.long "eventDelay"
       <> Opt.help ("Time delay (in ms) between processing delays.")
       <> Opt.showDefault
       <> Opt.value 0
       <> Opt.metavar "EVENTDELAY")
    <*> Opt.many Event.channelOptions
    <*> Opt.many Event.sourceIdOptions
    <*> Opt.optional (Event.UtcTime <$> Opt.option Opt.auto
        (  Opt.long "startTime"
             <> Opt.metavar "STARTTIME"
             <> Opt.help "Start time"))
    <*> Opt.optional (Event.UtcTime <$> Opt.option Opt.auto
        (  Opt.long "endTime"
             <> Opt.metavar "ENDTIME"
             <> Opt.help "End time"))

input :: Opt.Parser Input
input = C.subparserCmd "input ..." $ Opt.command "input" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data source")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *>
        (IFile <$> Encodings.encodeFormatOptions <*> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server")
        *> (IServer <$> Server.serverConnectionOptions)

output :: Opt.Parser Output
output = C.subparserCmd "output ..." $ Opt.command "output" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data destination")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *>
        (OFile <$> Encodings.encodeFormatOptions <*> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server")
        *> (OServer <$> Server.serverConnectionOptions)


-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do

    C.logM INFO $
        "command 'archive'" ++
        ", opts: " ++ show opts ++
        ", vcrOpts: " ++ show vcrOpts

    C.check (0 < chunkSize)
        "archive: Illegal chunk size."
    C.check (0 < maxEventSize)
        "archive: Illegal maximal event size."
    C.check (0 <= chunkDelay)
        "archive: Illegal chunk delay interval."
    C.check (0 <= eventDelay)
        "archive: Illegal event delay interval."

    runStream_ $
        source
        >-> delay eventDelay
        >-> traceDstEvents
        >-> destination

    C.logM INFO $
        "archive: done"

  where
    chunkSize = optChunkSize opts
    maxEventSize = optMaxEventSize opts
    chunkDelay = optChunkDelay opts
    eventDelay = optEventDelay opts

    source :: Producer Event.Event ()
    source = case optInput opts of
        IFile inpEnc inpFS ->
            File.fileReaderChunks chunkSize inpFS
            >-> delay chunkDelay
            >-> Encodings.fromByteString maxEventSize inpEnc
            >-> traceSrcEvents
            >-> channelFilters
            >-> sourceIdFilters
            >-> startTimeFilter
            >-> endTimeFilter
        IServer _inpSC -> undefined
      where

        traceSrcEvents ::
            Pipe (Either (String, ByteString) Event.Event) Event.Event ()
        traceSrcEvents = mkPipe $ \consume produce -> forever $ do
            result <- consume Clear
            case result of
                Left (msg,_) -> do
                    liftIO $ C.logM NOTICE $
                        "archive: Cannot decode an event: " ++ msg
                Right event -> do
                    liftIO $ C.logM DEBUG $
                        "archive: Src event " ++ show (Event.eUtcTime event)
                    produce event

        channelFilters :: Pipe Event.Event Event.Event ()
        channelFilters =
            case channels of
                [] -> Streams.map id
                _  -> Streams.filter ((`elem` channels).Event.eChannel)
            where channels = optChannelFilter opts

        sourceIdFilters :: Pipe Event.Event Event.Event ()
        sourceIdFilters =
            case sourceIds of
                [] -> Streams.map id
                _  -> Streams.filter ((`elem` sourceIds).Event.eSourceId)
            where sourceIds = optSourceIdFilter opts

        startTimeFilter :: Pipe Event.Event Event.Event ()
        startTimeFilter =
            case (optStartTime opts) of
                Just utcTime -> Streams.filter ((>=utcTime).Event.eUtcTime)
                Nothing -> Streams.map id

        endTimeFilter :: Pipe Event.Event Event.Event ()
        endTimeFilter =
            case (optEndTime opts) of
                Just utcTime -> Streams.filter ((>=utcTime).Event.eUtcTime)
                Nothing -> Streams.map id

    delay :: Int -> Pipe a a ()
    delay interval = mkPipe $ \ consume produce -> forever $ do
        msg <- consume Clear
        liftIO $ threadDelay (1000 * interval)
        produce msg

    traceDstEvents :: Pipe Event.Event Event.Event ()
    traceDstEvents = mkPipe $ \consume produce -> forever $ do
        event <- consume Clear
        liftIO $ C.logM DEBUG $
            "archive: Dst event " ++ show (Event.eUtcTime event)
        produce event

    destination :: Consumer Event.Event ()
    destination = case optOutput opts of
        OFile outEnc outFS ->
            Encodings.toByteString outEnc
            >-> File.fileWriter outFS Nothing (\_ -> return ())
        OServer _outSC -> undefined
