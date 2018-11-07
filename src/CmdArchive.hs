------------------
-- |
-- Module: CmdRecord
--
-- 'archive' command
--

{-# LANGUAGE LambdaCase #-}

module CmdArchive where

-- Standard imports.
import           Control.Concurrent (threadDelay)
import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import           Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(INFO, DEBUG, NOTICE))

-- Local imports.
import qualified Common as C
import           Common (logM)
import qualified Event
import qualified File
import qualified Encodings
import           Streams
import qualified Database as Db

cmdArchive :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdArchive = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event archiver")

-- | Archiver specific command options.
data Options = Options
    { optInput          :: Input
    , optOutput         :: Output
    , optChunkBytes     :: Int
    , optChunkEvents    :: Int
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
    | IDatabase Db.Db
    deriving (Eq, Show)

-- | Output options.
data Output
    = OFile Encodings.EncodeFormat File.FileStore
    | ODatabase Db.Db
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> input
    <*> output
    <*> Opt.option Opt.auto
        ( Opt.long "chunkBytes"
       <> Opt.help ("chunk size (bytes)")
       <> Opt.showDefault
       <> Opt.value (64*1024)
       <> Opt.metavar "BYTES")
    <*> Opt.option Opt.auto
        ( Opt.long "chunkEvents"
       <> Opt.help ("chunk size (events)")
       <> Opt.showDefault
       <> Opt.value (10000)
       <> Opt.metavar "BYTES")
    <*> Opt.option Opt.auto
        ( Opt.long "maxEventSize"
       <> Opt.help ("maximum size of event in bytes")
       <> Opt.showDefault
       <> Opt.value (64*1024)
       <> Opt.metavar "MAXEVENTSIZE")
    <*> Opt.option Opt.auto
        ( Opt.long "chunkDelay"
       <> Opt.help ("Time delay (in ms) between processing chunks.")
       <> Opt.showDefault
       <> Opt.value 0
       <> Opt.metavar "CHUNKDELAY")
    <*> Opt.option Opt.auto
        ( Opt.long "eventDelay"
       <> Opt.help ("Time delay (in ms) between processing events.")
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
    opts = file <|> database
    file = C.subparserCmd "file ..." $ Opt.command "file" $ Opt.info
        (IFile <$> Encodings.encodeFormatOptions <*> File.fileStoreOptions)
        (Opt.progDesc "Input from file")
    database = C.subparserCmd "database ..." $ Opt.command "database" $ Opt.info
        (IDatabase <$> Db.databaseConnectionOptions)
        (Opt.progDesc "Input from database")

output :: Opt.Parser Output
output = C.subparserCmd "output ..." $ Opt.command "output" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data destination")
  where
    opts = file <|> database
    file = C.subparserCmd "file ..." $ Opt.command "file" $ Opt.info
        (OFile <$> Encodings.encodeFormatOptions <*> File.fileStoreOptions)
        (Opt.progDesc "Output to file")
    database = C.subparserCmd "database ..." $ Opt.command "database" $ Opt.info
        (ODatabase <$> Db.databaseConnectionOptions)
        (Opt.progDesc "Output to database")

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do

    (startTimeUtc, _startTimeMono) <- Event.now
    logM INFO $ "startup " ++ show startTimeUtc
    logM INFO $
        "command 'archive'"
        ++ ", opts: " ++ show opts
        ++ ", vcrOpts: " ++ show vcrOpts

    C.check (0 < chunkBytes)
        "Illegal chunk size (bytes)."
    C.check (0 < chunkEvents)
        "Illegal chunk size (events)."
    C.check (0 < maxEventSize)
        "Illegal maximum event size."
    C.check (0 <= chunkDelay)
        "Illegal chunk delay interval."
    C.check (0 <= eventDelay)
        "Illegal event delay interval."

    runStream_ $
        source
        >-> delay eventDelay
        >-> traceDstEvents
        >-> destination

    logM INFO "done"

  where

    chunkBytes = optChunkBytes opts
    chunkEvents = optChunkEvents opts
    maxEventSize = optMaxEventSize opts
    chunkDelay = optChunkDelay opts
    eventDelay = optEventDelay opts

    source :: Producer Event.Event ()
    source = case optInput opts of
        IFile inpEnc inpFS ->
            File.fileReaderChunks chunkBytes inpFS
            >-> delay chunkDelay
            >-> Encodings.fromByteString maxEventSize inpEnc
            >-> traceSrcEvents
            >-> channelFilters
            >-> sourceIdFilters
            >-> startTimeFilter
            >-> endTimeFilter
        IDatabase db ->
            Db.databaseReaderTask db
                (optStartTime opts)
                (optEndTime opts)
                (optChannelFilter opts)
                (optSourceIdFilter opts)
      where

        traceSrcEvents ::
            Pipe (Either (String, ByteString) Event.Event) Event.Event ()
        traceSrcEvents = filterIO $ \case
            Left (msg,_) -> do
                logM NOTICE $ "Cannot decode an event: " ++ msg
                return Nothing
            Right event -> do
                logM DEBUG $ "Src event " ++ show (Event.eUtcTime event)
                return $ Just event

        channelFilters :: Pipe Event.Event Event.Event ()
        channelFilters = case channels of
            [] -> Streams.map id
            _  -> Streams.filter ((`elem` channels).Event.eChannel)
          where
            channels = optChannelFilter opts

        sourceIdFilters :: Pipe Event.Event Event.Event ()
        sourceIdFilters = case sourceIds of
            [] -> Streams.map id
            _  -> Streams.filter ((`elem` sourceIds).Event.eSourceId)
          where
            sourceIds = optSourceIdFilter opts

        startTimeFilter :: Pipe Event.Event Event.Event ()
        startTimeFilter = case (optStartTime opts) of
            Just utcTime -> Streams.filter ((>=utcTime).Event.eUtcTime)
            Nothing -> Streams.map id

        endTimeFilter :: Pipe Event.Event Event.Event ()
        endTimeFilter = case (optEndTime opts) of
            Just utcTime -> Streams.filter ((<utcTime).Event.eUtcTime)
            Nothing -> Streams.map id

    delay :: Int -> Pipe a a ()
    delay interval = filterIO $ \x -> do
        threadDelay (1000 * interval)
        return $ Just x

    traceDstEvents :: Pipe Event.Event Event.Event ()
    traceDstEvents = filterIO $ \event -> do
        logM DEBUG $ "Dst event " ++ show (Event.eUtcTime event)
        return $ Just event

    destination :: Consumer Event.Event ()
    destination = case optOutput opts of
        OFile outEnc outFS ->
            Encodings.toByteString outEnc
            >-> File.rotatingFileWriter (open outFS) Nothing (\_ -> return ())
        ODatabase db ->
            Db.databaseWriterTask chunkEvents db
      where
        open = \case
            File.FileStore "-" -> File.streamStdout
            File.FileStore fs -> File.streamHandle fs

