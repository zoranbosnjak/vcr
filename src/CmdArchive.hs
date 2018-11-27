------------------
-- |
-- Module: CmdRecord
--
-- 'archive' command
--

{-# LANGUAGE LambdaCase #-}

module CmdArchive where

-- Standard imports.
import           Control.Monad
import           Data.Bool
import           Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import           Control.Concurrent (threadDelay)
import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import           System.Log.Logger (Priority(INFO, DEBUG, NOTICE))
import qualified Data.Map as Map

import           Pipes
import qualified Pipes.Safe as PS
import qualified Pipes.Prelude as PP

-- Local imports.
import qualified Common as C
import           Common (logM)
import qualified Event
import qualified File
import qualified Encodings
import qualified Database as Db

-- | Input options.
data Input
    = IFile Encodings.EncodeFormat File.FileStore
    | IDatabase Db.Db
    deriving (Eq, Show)

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

-- | Output options.
data Output
    = OFile Encodings.EncodeFormat File.FileStore
    | ODatabase Db.Db
    deriving (Eq, Show)

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
    , optIgnoreError    :: Bool
    } deriving (Eq, Show)

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
        ( Opt.long "endTime"
             <> Opt.metavar "ENDTIME"
             <> Opt.help "End time"))
    <*> Opt.switch
        ( Opt.long "ignore"
            <> Opt.help "Proceed if [encoding, sequence...] error is detected"
            )

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    (startTimeUtc, _startTimeMono) <- Event.now
    logM INFO $ "startup " ++ show startTimeUtc
    logM INFO $
        "command 'archive'"
        ++ ", opts: " ++ show opts
        ++ ", vcrOpts: " ++ show vcrOpts

    (chunkBytes <= 0)   --> "Illegal chunk size (bytes)."
    (chunkEvents <= 0)  --> "Illegal chunk size (events)."
    (maxEventSize <= 0) --> "Illegal maximum event size."
    (chunkDelay < 0)    --> "Illegal chunk delay interval."
    (eventDelay < 0)    --> "Illegal event delay interval."

    PS.runSafeT $ runEffect $
        source
        >-> delay eventDelay
        >-> traceDstEvents
        >-> verifySequence
        >-> destination

    logM INFO "done"

  where

    condition --> e = when condition $ fail e

    chunkBytes = optChunkBytes opts
    chunkEvents = optChunkEvents opts
    maxEventSize = optMaxEventSize opts
    chunkDelay = optChunkDelay opts
    eventDelay = optEventDelay opts

    source :: Producer Event.Event (PS.SafeT IO) ()
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
        traceSrcEvents :: (MonadIO m) =>
            Pipe (Either (String, ByteString) Event.Event) Event.Event m r
        traceSrcEvents = forever $ await >>= \case
            Left (msg,s) -> do
                let e = "Cannot decode an event: " ++ msg ++ ", " ++ show s
                bool (fail e) (liftIO $ logM NOTICE e) (optIgnoreError opts)
            Right event -> do
                liftIO $ logM DEBUG $ "Src event "
                    ++ show (Event.eUtcTime event)
                yield event

        channelFilters :: (Monad m) => Pipe Event.Event Event.Event m r
        channelFilters = case channels of
            [] -> PP.map id
            _  -> PP.filter ((`elem` channels).Event.eChannel)
          where
            channels = optChannelFilter opts

        sourceIdFilters :: (Monad m) => Pipe Event.Event Event.Event m r
        sourceIdFilters = case sourceIds of
            [] -> PP.map id
            _  -> PP.filter ((`elem` sourceIds).Event.eSourceId)
          where
            sourceIds = optSourceIdFilter opts

        startTimeFilter :: (Monad m) => Pipe Event.Event Event.Event m r
        startTimeFilter = case (optStartTime opts) of
            Just utcTime -> PP.filter ((>=utcTime).Event.eUtcTime)
            Nothing -> PP.map id

        endTimeFilter :: (Monad m) => Pipe Event.Event Event.Event m r
        endTimeFilter = case (optEndTime opts) of
            Just utcTime -> PP.filter ((<utcTime).Event.eUtcTime)
            Nothing -> PP.map id

    delay :: (MonadIO m) => Int -> Pipe a a m r
    delay interval = PP.mapM $ \msg -> do
        liftIO $ threadDelay (1000 * interval)
        return msg

    traceDstEvents :: (MonadIO m) => Pipe Event.Event Event.Event m r
    traceDstEvents = PP.mapM $ \event -> do
        liftIO $ logM DEBUG $ "Dst event " ++ show (Event.eUtcTime event)
        return event

    -- TODO: verify monotonic time (no backward jump)
    verifySequence :: (MonadIO m) => Pipe Event.Event Event.Event m r
    verifySequence = loop Map.empty where
        loop tracks = do
            event <- await
            yield event
            let track = Event.eTrackId event
                sn = Event.eSequence event
                tracks' = Map.insert track sn tracks
                mErr = do
                    prev <- Map.lookup track tracks
                    let expected = Event.nextSequenceNum prev
                    guard $ expected /= sn
                    return $ "sequence error, expecting: " ++ show expected
                        ++ ", got: " ++ show event
            case mErr of
                Nothing -> return ()
                Just e -> do
                    bool (fail e) (liftIO $ logM NOTICE e) (optIgnoreError opts)
            loop tracks'

    destination :: Consumer Event.Event (PS.SafeT IO) ()
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

cmdArchive :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdArchive = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event archiver")

