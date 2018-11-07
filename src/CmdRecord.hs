------------------
-- |
-- Module: CmdRecord
--
-- 'record' command
--
-- TODO:
--  - IPv6 support on unicast/multicast input "[ipv6]", "ip" syntax

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdRecord (cmdRecord) where

-- standard imports
import           Control.Monad
import           Control.Monad.Fix
import           Data.Foldable (toList)
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
import           Data.String (fromString)
import           Data.Sequence as DS
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, (.=), object)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.HashMap.Strict as HMS
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.Async (withAsync)
import qualified System.Clock as Clk
import           System.Posix.Signals (installHandler, sigHUP, Handler(Catch))
import qualified Control.Exception as Ex
import qualified Data.Set as Set
import           Numeric (showFFloat)
import           Data.Text (unpack)
import qualified Data.Serialize as Bin
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Types

-- local imports
import qualified Common as C
import           Common (logM)
import qualified Event
import           Process
import           Streams
import qualified Udp
import qualified Encodings as Enc
import qualified File
import qualified Database as Db

cmdRecord :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdRecord = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event recorder")

type Ip = String
type Port = Int

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
    , optHttp       :: Maybe (Ip, Port)
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
    <*> Opt.optional httpOptions
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
    | OnOverflowFile FileOutput
    deriving (Generic, Eq, Show)

instance ToJSON OnOverflow where
    toJSON OnOverflowDrop = toJSON ("drop" :: String)
    toJSON (OnOverflowFile fo) = toJSON fo

instance FromJSON OnOverflow where
    parseJSON (Data.Aeson.String s) = case (words $ unpack s) of
        ("drop":[]) -> return OnOverflowDrop
        _ -> typeMismatch "OnOverflow" (Data.Aeson.String s)
    parseJSON val@(Data.Aeson.Object _) = OnOverflowFile <$> parseJSON val
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
onOverflowFile = tag *> fmap OnOverflowFile opts
  where
    tag = Opt.flag' ()
        ( Opt.long "dropFile"
       <> Opt.help "In case of buffer overflow, save data to a file"
        )
    opts = FileOutput
        <$> File.fileStoreOptions
        <*> Enc.encodeFormatOptions
        <*> Opt.optional File.rotateOptions

sendEvents :: Opt.Parser Int
sendEvents = Opt.option Opt.auto
    ( Opt.long "sendEvents"
   <> Opt.metavar "N"
   <> Opt.help "Send events in batches of N"
   <> Opt.value 1000
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
   <> Opt.help "Drop events when buffer size reaches N"
   <> Opt.value 10000
   <> Opt.showDefault
    )

httpOptions :: Opt.Parser (Ip, Port)
httpOptions = C.subparserCmd "http ..." $
    Opt.command "http" $ Opt.info
        (opts <**> Opt.helper)
        (Opt.progDesc "Enable http server")
  where
    opts = (,)
        <$> parseIp
        <*> parsePort
    parseIp = Opt.strOption
        ( Opt.long "ip"
       <> Opt.metavar "IP"
       <> Opt.help "Ip address"
       <> Opt.value "127.0.0.1"
       <> Opt.showDefault
        )
    parsePort = Opt.option Opt.auto
        ( Opt.long "port"
       <> Opt.metavar "PORT"
       <> Opt.help "TCP port number"
       <> Opt.value 8080
       <> Opt.showDefault
        )

emptyConfig :: AppConfig
emptyConfig = AppConfig
    { appStdin = Nothing
    , appInputs = mempty
    , appStdout = False
    , appFileOutput = Nothing
    , appDatabaseOutput = Nothing
    }

stdInInput :: STM (Maybe Event.Channel) -> Event.SourceId -> Event.SessionId
    -> Producer Event.Event ()
stdInInput cfg recId sessionId = mkProducer $ \produce -> do
  withVar "stdin" cfg $ \case
    Nothing -> doNothing
    Just ch -> do
        trackId <- Event.trackId . Data.UUID.toString <$> nextRandom
        logM INFO $ show ch ++ " -> " ++ show trackId
        let gen = mkGenPipeIO minBound $ \s sn -> do
                (tUtc, tMono) <- Event.now
                return
                    ( Event.Event ch recId tUtc tMono sessionId trackId sn s
                    , Event.nextSequenceNum sn
                    )
            p = File.fileReaderLines (File.FileStore "-") >-> gen
        (streamAction p) noConsume produce

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
        withAsync (act (ch, param)) $ \a -> do
            rv <- Async.waitCatch a
            let msg = case rv of
                    Left e -> "with exception: " ++ show e
                    Right _ -> "without exception"
            logM NOTICE $ "Input terminated: "
                ++ show ch ++ ", " ++ show param ++ ", " ++ msg
                ++ ", will auto restart in few moments..."
            C.threadDelaySec 3
            logM INFO $ "Auto restart input: " ++ show ch ++ ", " ++ show param

udpInputs :: STM Inputs -> Event.SourceId -> Event.SessionId
    -> Producer Event.Event ()
udpInputs getInputs recId sessionId = mkProducer $ \produce -> do
    processInputs getInputs $ \(ch, udp) -> do
        trackId <- Event.trackId . Data.UUID.toString <$> nextRandom
        logM INFO $ show ch ++ " -> " ++ show trackId
        let gen = mkGenPipeIO minBound $ \(val, _addr) sn -> do
                (tUtc, tMono) <- Event.now
                return
                    ( Event.Event ch recId tUtc tMono sessionId trackId sn val
                    , Event.nextSequenceNum sn
                    )
            p = Udp.udpReader udp >-> gen
        (streamAction p) noConsume produce

stdOutOutput :: (Show a, Bin.Serialize a, ToJSON a) => STM Bool -> Consumer a ()
stdOutOutput cfg = mkConsumer $ \consume ->
    withVar "stdout" cfg $ \case
        False -> (streamAction drain) consume noProduce
        True -> (streamAction consumer) consume noProduce
  where
    consumer =
        Streams.map (Enc.encode Enc.EncText)
        >-> File.rotatingFileWriter File.streamStdout Nothing (\_ -> return ())

fileOutput :: (Show a, Bin.Serialize a, ToJSON a) =>
    STM (Maybe FileOutput) -> Consumer a ()
fileOutput cfg = mkConsumer $ \consume ->
    withVar "file output" cfg $ \case
        Nothing -> (streamAction drain) consume noProduce
        Just fo -> (streamAction (consumer fo)) consume noProduce
  where
    consumer fo =
        Streams.map (Enc.encode $ outFileEnc fo)
        >-> File.rotatingFileWriter (open fo) (outFileRotate fo) (logM INFO)
    open f = case outFileStore f of
        File.FileStore "-" -> File.streamStdout
        File.FileStore fs -> File.streamHandle fs

databaseOutput :: Event.SessionId -> (Db.Status -> STM ())
    -> STM (Maybe DatabaseOutput) -> Consumer Event.Event ()
databaseOutput sessionId setStat cfg = mkConsumer $ \consume ->
    withVar "database output" cfg $ \case
        Nothing -> (streamAction drain) consume noProduce
        Just dbo -> do
            qOfw <- newTVarIO DS.empty  -- overflow queue
            dbQueue <- newEmptyTMVarIO
            let dropEvents s = modifyTVar qOfw (|> s)
                dbConsume = takeTMVar dbQueue
                dbProduce = putTMVar dbQueue
                conn = outDatabase dbo
                bufSend = outDatabaseBufferSend dbo
                bufDropAt = outDatabaseBufferDrop dbo
                onOwf = outDatabaseOnOverflow dbo
                dbWriter =
                    Db.databaseWriter setStat bufSend bufDropAt dropEvents conn
            -- run database writer and drop handler concurrently
            -- drop handler will finish only after database writer,
            -- so just wait for a drop handler to finish
            withAsync ((streamAction dbWriter) dbConsume noProduce) $ \a1 ->
                withAsync (dropHandler qOfw a1 onOwf) $ \a2 -> do
                    let runConsume = atomically $ do
                            val <- consume
                            dbProduce val
                            return val
                        onEnd rv = Async.wait a2 >> return rv
                        onData _msg = return ()
                    processMessage runConsume onEnd onData
  where
    dropHandler q db onDrop = runStream_ $ producer >-> consumer where
        producer = mkProducer $ \produce -> fix $ \loop -> do
            -- wait for
            --  - empty queue and finished database process
            --  - or some messages to drop
            mMsgs <- atomically $ do
                finished <- Async.pollSTM db >>= \case
                    Nothing -> return False
                    Just (Left e) -> Ex.throw e
                    Just (Right _) -> return True
                msgs <- readTVar q
                case finished && (DS.null msgs) of
                    True -> return Nothing
                    False -> case DS.null msgs of
                        True -> retry
                        False -> return $ Just msgs
            case mMsgs of
                Nothing -> return ()
                Just msgs -> do
                    mapM_ (atomically . produce) $ toList msgs
                    loop
        consumer = case onDrop of
            OnOverflowDrop -> drain
            OnOverflowFile fo ->
                let (Event.SessionId sid) = sessionId
                    fs1 = outFileStore fo
                    fs2 = File.filePath fs1 ++ "-" ++ sid
                    open = case fs1 of
                        File.FileStore "-" -> File.streamStdout
                        File.FileStore _ -> File.streamPath fs2
                in
                    Streams.map (Enc.encode $ outFileEnc fo)
                    >-> File.rotatingFileWriter open (outFileRotate fo)
                        (logM INFO)

logUptime :: Event.MonoTime -> Double -> IO b
logUptime startTimeMono t = forever $ do
    C.threadDelaySec t
    (_t1, t2) <- Event.now
    let uptimeSec =
            Event.monoTimeToSeconds t2 - Event.monoTimeToSeconds startTimeMono
        uptimeDays = uptimeSec / (24*3600)
    logM INFO $
        "uptime: " ++ showFFloat (Just 3) uptimeDays "" ++ " days"

startHttp :: Event.UtcTime -> Event.MonoTime -> TVar (Maybe Db.Status)
    -> (String, Warp.Port) -> IO ()
startHttp startTimeUtc startTimeMono dbStatV (ip, port) = do
    let settings =
            Warp.setPort port $
            Warp.setHost (fromString ip) $
            Warp.defaultSettings
    logM INFO $ "http server, ip: " ++ show ip ++ ", port: " ++ show port
    Warp.runSettings settings $ app
  where
    app request respond = withLog go
      where
        withLog act = case parseMethod (requestMethod request) of
            Left _ -> respond $ responseLBS
                status400
                [("Content-Type", "text/plain")]
                "400 - unknown request method\n"
            Right mtd -> do
                logM DEBUG $ show request
                act (pathInfo request) mtd

        go ["startTime"] GET = respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (BSL8.pack $ show startTimeUtc ++ "\n")

        go ["uptime"] GET = do
            (_tUtc, tMono) <- Event.now
            let uptimeNsec = Clk.toNanoSecs $ Clk.diffTimeSpec
                        (Event.getMonoTime tMono)
                        (Event.getMonoTime startTimeMono)
                uptimeSec = uptimeNsec `div` (10^(9::Int))
                uptimeDays :: Double
                uptimeDays = fromIntegral uptimeSec / (24*3600)
                uptimeDaysStr = showFFloat (Just 3) uptimeDays "" ++ " days\n"
            respond $ responseLBS status200 [("Content-Type", "text/plain")]
                (BSL8.pack $ uptimeDaysStr)

        go ["status"] GET = do
            dbStat <- atomically $ readTVar dbStatV
            let stat = object
                    [ "database"    .= dbStat
                    ]
            respond $ responseLBS status200 [("Content-Type", "text/plain")]
                (Enc.jsonEncodePretty stat <> "\n")

        -- not found
        go _ _ = respond $ responseLBS
            status404
            [("Content-Type", "text/plain")]
            "404 - Not Found\n"

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
      Nothing -> do
        dbStat <- newTVarIO Nothing
        let updateDbStatus = writeTVar dbStat . Just
            rtCheck = expedite 3.0 $ fail "real time error"
        runAll
            [ process_ "uptime" $
                maybe doNothing (logUptime startTimeMono) (optUptime opts)
            , process_ "http" $ maybe doNothing
                (startHttp startTimeUtc startTimeMono dbStat)
                (optHttp opts)
            , process_ "data flow" $ runStream_ $
                mergeProducers  -- inputs
                    [ stdInInput (appStdin <$> readTVar cfg) recId sessionId
                        >-> rtCheck
                    , udpInputs (appInputs <$> readTVar cfg) recId sessionId
                        >-> rtCheck
                    ]
                >-> forkConsumers   -- outputs
                    [ stdOutOutput (appStdout <$> readTVar cfg)
                    , fileOutput (appFileOutput <$> readTVar cfg)
                    , databaseOutput sessionId updateDbStatus
                        (appDatabaseOutput <$> readTVar cfg)
                    ]
            ]

