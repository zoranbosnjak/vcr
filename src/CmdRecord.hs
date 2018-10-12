------------------
-- |
-- Module: CmdRecord
--
-- 'record' command
--
-- TODO:
--  - IPv6 support on unicast/multicast input "[ipv6]", "ip" syntax

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module CmdRecord (cmdRecord) where

-- standard imports
import           Control.Monad
import           Control.Monad.Fix
import           Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(..))
import           Data.Text (pack)
import           Network.BSD (getHostName)
import           Data.Maybe (fromMaybe)
import qualified Data.UUID
import           Data.UUID.V4 (nextRandom)
import           Control.Concurrent.STM hiding (check)
import qualified Data.Map as Map
import           GHC.Generics (Generic)
import qualified Data.Aeson
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, (.=), object)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMS
import qualified Control.Concurrent.Async as Async
import           System.Posix.Signals (installHandler, sigHUP, Handler(Catch))
import qualified Control.Exception as Ex
import qualified Data.Set as Set
import           Numeric (showFFloat)
import           Data.Text (unpack)
import qualified Data.Serialize as Bin

-- local imports
import qualified Common as C
import qualified Event
import           Common (logM)
import           Process
import qualified Udp
import qualified Encodings as Enc
import qualified File
import qualified Database as Db

cmdRecord :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdRecord = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event recorder")

-- | Top level commands.
data TopCommand
    = TopFile FilePath
    | TopCmd CmdLine
    | TopDump FilePath
    | TopPrepare Db.Db
    deriving (Eq, Show)

-- | Speciffic command options.
data Options = Options
    { optIdent      :: Maybe Event.SourceId
    , optUptime     :: Maybe Double
    , optArgs       :: TopCommand
    } deriving (Eq, Show)

-- | Options if arguments are given in command line.
data CmdLine = CmdLine
    { optStdIn      :: Maybe Event.Channel
    , optUdpIn      :: Map.Map Event.Channel Udp.UdpIn
    , optStdOut     :: Bool
    , optFileOut    :: Maybe FileOutput
    , optDatabaseOut :: Maybe DatabaseOutput
    , optBootstrap  :: Bool
    } deriving (Eq, Show)

-- | File output.
data FileOutput = FileOutput
    { outFileStore  :: File.FileStore
    , outFileEnc    :: Enc.EncodeFormat
    , outFileRotate :: (Maybe File.Rotate)
    } deriving (Generic, Eq, Show)

instance ToJSON FileOutput
instance FromJSON FileOutput

-- | Eventual application configuration.
data AppConfig = AppConfig
    { appStdin :: Maybe Event.Channel
    , appInputs :: Inputs
    , appStdout :: Bool
    , appFileOutput :: Maybe FileOutput
    , appDatabaseOutput :: Maybe DatabaseOutput
    } deriving (Generic, Eq, Show)

instance ToJSON AppConfig
instance FromJSON AppConfig

newtype Inputs = Inputs (Map.Map Event.Channel Udp.UdpIn)
    deriving (Generic, Eq, Show)

instance Semigroup Inputs where
    (Inputs i1) <> (Inputs i2) = Inputs (mappend i1 i2)

instance Monoid Inputs where
    mempty = Inputs mempty
    mappend = (<>)

instance ToJSON Inputs where
    toJSON (Inputs val) = object
        [ key .= toJSON i | (Event.Channel key, i) <- Map.toList val ]

instance FromJSON Inputs where
    parseJSON (Data.Aeson.Object o) = do
        lst <- sequence $ HMS.map parseJSON o
        return $ Inputs $ Map.fromList
            [(Event.Channel key, i) | (key, i) <- HMS.toList lst]
    parseJSON t = typeMismatch "Inputs" t

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> Opt.optional Event.sourceIdOptions
    <*> Opt.optional ( Opt.option Opt.auto
        ( Opt.long "logUptime"
       <> Opt.help "Log uptime every N seconds"
       <> Opt.metavar "N"
        ))
    <*> parseConfig
  where
    parseConfig =
        ( C.subparserCmd "config ..." $ Opt.command "config" $ Opt.info
            (fmap TopFile parseConfigFile <**> Opt.helper)
            (Opt.progDesc "Use config file")
        )
       <|> ( C.subparserCmd "args ..." $ Opt.command "args" $ Opt.info
            (fmap TopCmd parseConfigArgs <**> Opt.helper)
            (Opt.progDesc "Use command line arguments")
           )
       <|> ( C.subparserCmd "dump FILE" $ Opt.command "dump" $ Opt.info
            (fmap TopDump parseConfigFile <**> Opt.helper)
            (Opt.progDesc "Read and print config file")
           )
       <|> ( C.subparserCmd "prepare ..." $ Opt.command "prepare" $ Opt.info
            (fmap TopPrepare Db.databaseConnectionOptions <**> Opt.helper)
            (Opt.progDesc "Create database tables")
           )

    parseConfigFile :: Opt.Parser FilePath
    parseConfigFile = Opt.argument Opt.str (Opt.metavar "FILE")

    parseConfigArgs :: Opt.Parser CmdLine
    parseConfigArgs = CmdLine
        <$> Opt.optional stdinOptions
        <*> (Map.fromList <$> Opt.many udpInputOptions)
        <*> (maybe False (const True) <$> Opt.optional stdoutOptions)
        <*> Opt.optional storeFileOptions
        <*> Opt.optional storeDatabaseOptions
        <*> Opt.switch
            ( Opt.long "bootstrap"
            <> Opt.help "Print config file and exit"
            )

stdinOptions :: Opt.Parser Event.Channel
stdinOptions = C.subparserCmd "stdin ..." $ Opt.command "stdin" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Read data from stdin")
  where
    opts = Event.channelOptions

udpInputOptions :: Opt.Parser (Event.Channel, Udp.UdpIn)
udpInputOptions = C.subparserCmd "udpin ..." $ Opt.command "udpin" $ Opt.info
        (opts <**> Opt.helper)
        (Opt.progDesc "Read data from UDP")
  where
    opts = (,) <$> Event.channelOptions <*> Udp.udpInOptions

stdoutOptions :: Opt.Parser ()
stdoutOptions = C.subparserCmd "stdout" $ Opt.command "stdout" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Write data to stdout")
  where
    opts = pure ()

storeFileOptions :: Opt.Parser FileOutput
storeFileOptions = C.subparserCmd "fileout ..." $ Opt.command "fileout" $
    Opt.info
        (opts <**> Opt.helper)
        (Opt.progDesc "Store data to a (rotating) file")
  where
    opts = FileOutput
        <$> File.fileStoreOptions
        <*> Enc.encodeFormatOptions
        <*> Opt.optional File.rotateOptions

-- | Database output.
data DatabaseOutput = DatabaseOutput
    { outDatabase           :: Db.Db
    , outDatabaseOnOverflow :: OnOverflow
    , outDatabaseBufferSend :: (Int, Double)
    , outDatabaseBufferDrop :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON DatabaseOutput
instance FromJSON DatabaseOutput

-- | Overflow handler (what to do in case of buffer overflow).
data OnOverflow
    = OnOverflowDrop
    | OnOverflowFile File.FileStore
    deriving (Generic, Eq, Show)

instance ToJSON OnOverflow where
    toJSON OnOverflowDrop = toJSON ("drop" :: String)
    toJSON (OnOverflowFile (File.FileStore f)) = toJSON $ "file " ++ f

instance FromJSON OnOverflow where
    parseJSON (Data.Aeson.String s) = case (words $ unpack s) of
        ("drop":[]) -> return OnOverflowDrop
        ("file":f:[]) -> return $ OnOverflowFile $ File.FileStore f
        _ -> typeMismatch "OnOverflow" (Data.Aeson.String s)
    parseJSON t = typeMismatch "OnOverflow" t

storeDatabaseOptions :: Opt.Parser DatabaseOutput
storeDatabaseOptions = C.subparserCmd "databaseout ..." $
    Opt.command "databaseout" $ Opt.info
        (opts <**> Opt.helper)
        (Opt.progDesc "Store data to database")
  where
    opts = DatabaseOutput
        <$> Db.databaseConnectionOptions
        <*> (onOverflowDrop <|> onOverflowFile)
        <*> ((,) <$> sendEvents <*> sendSeconds)
        <*> bufferDropAt

onOverflowDrop :: Opt.Parser OnOverflow
onOverflowDrop = Opt.flag' OnOverflowDrop
    ( Opt.long "dropDiscard"
   <> Opt.help "In case of buffer overflow, drop data"
    )

onOverflowFile :: Opt.Parser OnOverflow
onOverflowFile = OnOverflowFile <$> Opt.strOption
    ( Opt.long "dropFile"
   <> Opt.metavar "FILE"
   <> Opt.help "In case of buffer overflow, save data to a file"
    )

sendEvents :: Opt.Parser Int
sendEvents = Opt.option Opt.auto
    ( Opt.long "sendEvents"
   <> Opt.metavar "N"
   <> Opt.help "Send events in batches of N"
   <> Opt.value 10000
   <> Opt.showDefault
    )

sendSeconds :: Opt.Parser Double
sendSeconds = Opt.option Opt.auto
    ( Opt.long "sendSeconds"
   <> Opt.metavar "SECONDS"
   <> Opt.help "Send events at least every N seconds"
   <> Opt.value 1.0
   <> Opt.showDefault
    )

bufferDropAt :: Opt.Parser Int
bufferDropAt = Opt.option Opt.auto
    ( Opt.long "dropAt"
   <> Opt.metavar "N"
   <> Opt.help "Size of buffer in terms of 'sendEvents' size"
   <> Opt.value 100
   <> Opt.showDefault
    )

-- | Process inputs.
processInputs :: STM Inputs -> ((Event.Channel, Udp.UdpIn) -> IO ()) -> IO ()
processInputs getInputs act = do
    active <- newTVarIO mempty
    loop active mempty `Ex.finally` terminate active
  where
    terminate active = do
        s <- atomically $ readTVar active
        mapM_ Async.cancel s

    loop active current = do
        (removed, added, changed) <- atomically $ do
            (Inputs newCfg) <- getInputs
            let removed = current `Map.difference` newCfg
                added = newCfg `Map.difference` current
                checkF k v = do
                    oldParam <- snd <$> Map.lookup k current
                    newParam <- Map.lookup k newCfg
                    guard $ newParam /= oldParam
                    return (v, newParam)
                changed = Map.mapMaybeWithKey checkF current

            when (and [Map.null removed, Map.null added, Map.null changed])
                retry
            return (removed, added, changed)

        logM INFO $ "Input config changed"
        logM INFO $ "removed: " ++ show (Map.keys removed)
        logM INFO $ "added: "   ++ show (Map.keys added)
        logM INFO $ "changed: " ++ show (Map.keys changed)

        stopped   <- Map.traverseWithKey (onStop active) removed
        started   <- Map.traverseWithKey (onStart active) added
        restarted <- Map.traverseWithKey (onRestart active) changed

        let newRunning =
                Map.union restarted
                . Map.union started
                . flip Map.difference stopped
                $ current

        loop active newRunning

    onStart active ch param = do
        logM INFO $ "Start input: " ++ show ch ++ ", " ++ show param
        a <- Ex.mask $ \restore -> do
            a <- Async.async $ restore $ autoRestart ch param
            atomically $ modifyTVar' active $ Set.insert a
            return a
        return (a, param)

    onStop active ch (a, param) = do
        logM INFO $ "Stop input: " ++ show ch ++ ", " ++ show param
        Ex.mask $ \_restore -> do
            atomically $ modifyTVar' active $ Set.delete a
            Async.cancel a

    onRestart active ch (running, newParam) = do
        onStop active ch running
        onStart active ch newParam

    autoRestart ch param = forever $
        Async.withAsync (act (ch, param)) $ \a -> do
            rv <- Async.waitCatch a
            let msg = case rv of
                    Left e -> "with exception: " ++ show e
                    Right _ -> "without exception"
            logM NOTICE $ "Input terminated: "
                ++ show ch ++ ", " ++ show param ++ ", " ++ msg
                ++ ", will auto restart in few moments..."
            C.threadDelaySec 3
            logM INFO $ "Auto restart input: " ++ show ch ++ ", " ++ show param

emptyConfig :: AppConfig
emptyConfig = AppConfig
    { appStdin = Nothing
    , appInputs = mempty
    , appStdout = False
    , appFileOutput = Nothing
    , appDatabaseOutput = Nothing
    }

stdInInput :: STM (Maybe Event.Channel) -> Event.SourceId -> Event.SessionId
    -> Producer Event.Event
stdInInput cfg recId sessionId = Producer $ \produce -> do
  withVar "stdin" cfg $ \case
    Nothing -> doNothing
    Just ch -> do
        trackId <- Event.trackId . Data.UUID.toString <$> nextRandom
        logM INFO $ show ch ++ " -> " ++ show trackId
        let consumer = Consumer $ \consume -> (fix $ \loop sn -> do
                atomically consume >>= \case
                    EndOfData -> return ()
                    Message val -> do
                        (tUtc, tMono) <- Event.now
                        atomically $ produce $ Event.Event
                            ch recId tUtc tMono sessionId trackId sn val
                        loop $ Event.nextSequenceNum sn
                ) minBound
        (File.fileReaderLines (File.FileStore "-")) >-> consumer

udpInput :: STM Inputs -> Event.SourceId -> Event.SessionId
    -> Producer Event.Event
udpInput getInputs recId sessionId = Producer $ \produce -> do
    processInputs getInputs $ \(ch, udp) -> do
        trackId <- Event.trackId . Data.UUID.toString <$> nextRandom
        logM INFO $ show ch ++ " -> " ++ show trackId
        let consumer = Consumer $ \consume -> (fix $ \loop sn -> do
                atomically consume >>= \case
                    EndOfData -> return ()
                    Message (val, _addr) -> do
                        (tUtc, tMono) <- Event.now
                        _ <- atomically $ produce $ Event.Event
                            ch recId tUtc tMono sessionId trackId sn val
                        loop $ Event.nextSequenceNum sn
                ) minBound
        (Udp.udpReader udp) >-> consumer

stdOutOutput :: (Show a, Bin.Serialize a, ToJSON a) => STM Bool -> Consumer a
stdOutOutput cfg = Consumer $ \consume ->
    withVar "stdout" cfg $ \case
        False -> drain consume
        True -> getConsumer consumer $ consume
  where
    consumer = mapConsumer (Enc.encode Enc.EncText) $
        File.fileWriter (File.FileStore "-") Nothing (\_ -> return ())

fileOutput :: (Show a, Bin.Serialize a, ToJSON a) =>
    STM (Maybe FileOutput) -> Consumer a
fileOutput cfg = Consumer $ \consume ->
    withVar "file output" cfg $ \case
        Nothing -> drain consume
        Just fo -> getConsumer (consumer fo) $ consume
  where
    consumer fo = mapConsumer (Enc.encode $ outFileEnc fo) $
        File.fileWriter (outFileStore fo) (outFileRotate fo) (logM INFO)

databaseOutput :: STM (Maybe DatabaseOutput) -> Event.SessionId
    -> Consumer Event.Event
databaseOutput cfg sessionId = Consumer $ \consume ->
    withVar "database output" cfg $ \case
        Nothing -> drain consume
        Just dbo -> do
            let conn = outDatabase dbo
                bufSend = outDatabaseBufferSend dbo
                bufDropAt = outDatabaseBufferDrop dbo
                dropAct events = case outDatabaseOnOverflow dbo of
                    OnOverflowDrop -> do
                        logM NOTICE $ "buffer overflow, dropping messages"
                    OnOverflowFile fs -> do
                        let (Event.SessionId sid) = sessionId
                            fn = File.filePath fs ++ "-" ++ sid
                        logM NOTICE $
                            "buffer overflow, saving messages to a file "
                            ++ show fn
                        BS.appendFile fn $ Enc.encodeList Enc.EncJSON events
                consumer = Db.databaseWriter bufSend bufDropAt dropAct conn
            getConsumer consumer consume

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = case optArgs opts of

  TopDump f -> do
    s <- BSL.readFile f
    case Enc.jsonEitherDecode s of
        Right val -> do
            BSL.putStr $ Enc.jsonEncodePretty (val :: AppConfig)
        Left e -> fail e

  TopPrepare db -> do
    Db.prepareDatabase db

  _ -> do
    (startTimeUtc, startTimeMono) <- Event.now
    logM INFO $ "startup " ++ show startTimeUtc
    logM INFO $
        "command 'record', opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts
    recId <- do
        hostname <- Event.sourceId . pack <$> getHostName
        return $ fromMaybe hostname $ optIdent opts
    logM INFO $ show recId

    sessionId <- Event.sessionId . Data.UUID.toString <$> nextRandom
    logM INFO $ show sessionId

    -- Global configuration
    cfg <- newTVarIO emptyConfig

    doBootstrap <- case optArgs opts of

        -- dynamic config from file
        TopFile f -> do
            let readConfigFile = do
                    rv <- Ex.try $ do
                        s <- BSL.readFile f
                        case Enc.jsonEitherDecode s of
                            Right val -> do
                                logM INFO $ "Reading config success: "
                                    ++ show val
                                atomically $ writeTVar cfg val
                            Left e -> fail e
                    case rv of
                        Left (e :: Ex.SomeException) -> do
                            logM NOTICE $ "Reading config failure: " ++ show e
                        Right _ -> return ()
                handler = Catch $ do
                    logM INFO "SIGHUP received"
                    readConfigFile
            readConfigFile
            _ <- installHandler sigHUP handler Nothing
            return Nothing

        -- static config from command line
        TopCmd args -> do
            logM INFO $ show args
            let app = AppConfig
                    { appStdin = optStdIn args
                    , appInputs = Inputs $ optUdpIn args
                    , appStdout = optStdOut args
                    , appFileOutput = optFileOut args
                    , appDatabaseOutput = optDatabaseOut args
                    }
            atomically $ writeTVar cfg app
            return $ case optBootstrap args of
                False -> Nothing
                True -> Just app

        -- should not be here
        cmd -> fail $ "unexpected command " ++ show cmd

    case doBootstrap of
      Just val -> BSL.putStr $ Enc.jsonEncodePretty val
      Nothing -> runAll
        [ process_ "uptime" $ case optUptime opts of
            Nothing -> doNothing
            Just t -> forever $ do
                C.threadDelaySec t
                (_t1, t2) <- Event.now
                let uptimeSec =
                        Event.monoTimeToSeconds t2
                        - Event.monoTimeToSeconds startTimeMono
                    uptimeDays = uptimeSec / (24*3600)
                logM INFO $
                    "uptime: " ++ showFFloat (Just 3) uptimeDays "" ++ " days"

        , process_ "data flow" $ runFlow
            -- inputs
            [ stdInInput (appStdin <$> readTVar cfg) recId sessionId
            , udpInput (appInputs <$> readTVar cfg) recId sessionId
            ]
            -- outputs
            [ stdOutOutput (appStdout <$> readTVar cfg)
            , fileOutput (appFileOutput <$> readTVar cfg)
            , databaseOutput (appDatabaseOutput <$> readTVar cfg) sessionId
            ]
        ]

