------------------
-- |
-- Module: CmdRecord
--
-- 'record' command
--
-- TODO: - IPv6 support on unicast/multicast input "[ipv6]", "ip" syntax

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module CmdRecord (cmdRecord) where

-- standard imports
import           Control.Monad
import           Control.Monad.Fix
import           Control.DeepSeq
import qualified Options.Applicative as Opt
import           Options.Applicative ((<**>), (<|>))
import           System.Log.Logger (Priority(..))
import           Data.Text as Text
import           Data.Bool
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
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Types

import           Pipes
import qualified Pipes.Safe as PS
import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS

-- local imports
import qualified Common as C
import           Common (logM)
import qualified Event
import           Process
import qualified Udp
import qualified Encodings as Enc
import qualified File
import qualified Database as Db

type Ip = String
type Port = Int

newtype Inputs = Inputs (Map.Map Event.Channel Udp.UdpIn)
    deriving (Generic, Eq, Show)

instance ToJSON Inputs where
    toJSON (Inputs val) = object
        [ key .= toJSON i | (Event.Channel key, i) <- Map.toList val ]

instance FromJSON Inputs where
    parseJSON (Data.Aeson.Object o) = do
        lst <- sequence $ HMS.map parseJSON o
        return $ Inputs $ Map.fromList
            [(Event.Channel key, i) | (key, i) <- HMS.toList lst]
    parseJSON t = typeMismatch "Inputs" t

inputsOptions :: Opt.Parser Inputs
inputsOptions = fmap Inputs (Map.fromList <$> Opt.many udpInputOptions)
  where
    udpInputOptions :: Opt.Parser (Event.Channel, Udp.UdpIn)
    udpInputOptions =
        C.subparserCmd "udpin ..." $ Opt.command "udpin" $ Opt.info
            (opts <**> Opt.helper)
            (Opt.progDesc "Read data from UDP")
      where
        opts = (,) <$> Event.channelOptions <*> Udp.udpInOptions

-- | File output.
data FileOutput = FileOutput
    { outFileStore  :: File.FileStore
    , outFileEnc    :: Enc.EncodeFormat
    , outFileRotate :: (Maybe File.Rotate)
    } deriving (Generic, Eq, Show)

instance ToJSON FileOutput
instance FromJSON FileOutput

-- | Overflow handler (what to do in case of buffer overflow).
data OnOverflow
    = OnOverflowDrop
    | OnOverflowFile FileOutput
    deriving (Generic, Eq, Show)

instance ToJSON OnOverflow where
    toJSON OnOverflowDrop = toJSON ("drop" :: String)
    toJSON (OnOverflowFile fo) = toJSON fo

instance FromJSON OnOverflow where
    parseJSON (Data.Aeson.String s) = case (Prelude.words $ Text.unpack s) of
        ("drop":[]) -> return OnOverflowDrop
        _ -> typeMismatch "OnOverflow" (Data.Aeson.String s)
    parseJSON val@(Data.Aeson.Object _) = OnOverflowFile <$> parseJSON val
    parseJSON t = typeMismatch "OnOverflow" t

-- | Database output.
data DatabaseOutput = DatabaseOutput
    { outDatabase           :: Db.Db
    , outDatabaseOnOverflow :: OnOverflow
    , outDatabaseBufferSend :: Db.Buffer
    , outDatabaseBufferDrop :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON DatabaseOutput
instance FromJSON DatabaseOutput

databaseOutputOptions :: Opt.Parser DatabaseOutput
databaseOutputOptions = C.subparserCmd "databaseout ..." $
    Opt.command "databaseout" $ Opt.info
        (opts <**> Opt.helper)
        (Opt.progDesc "Store data to database")
  where
    opts = DatabaseOutput
        <$> Db.databaseConnectionOptions
        <*> (onOverflowDrop <|> onOverflowFile)
        <*> (Db.Buffer <$> sendEvents <*> sendSeconds)
        <*> bufferDropAt

    onOverflowDrop :: Opt.Parser OnOverflow
    onOverflowDrop = Opt.flag' OnOverflowDrop
        ( Opt.long "dropDiscard"
       <> Opt.help "In case of buffer overflow, drop data"
        )

    onOverflowFile :: Opt.Parser OnOverflow
    onOverflowFile = tag *> fmap OnOverflowFile optsFo
      where
        tag = Opt.flag' ()
            ( Opt.long "dropFile"
           <> Opt.help "In case of buffer overflow, save data to a file"
            )
        optsFo = FileOutput
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

-- | Eventual application configuration.
data AppConfig = AppConfig
    { appInputs :: Inputs
    , appFileOutput :: Maybe FileOutput
    , appDatabaseOutput :: Maybe DatabaseOutput
    } deriving (Generic, Eq, Show)

instance ToJSON AppConfig
instance FromJSON AppConfig

-- | Options if arguments are given in command line.
data CmdLine = CmdLine
    { optInputs     :: Inputs
    , optFileOut    :: Maybe FileOutput
    , optDatabaseOut :: Maybe DatabaseOutput
    , optBootstrap  :: Bool
    } deriving (Eq, Show)

cmdLineOpts :: Opt.Parser CmdLine
cmdLineOpts = CmdLine
    <$> inputsOptions
    <*> Opt.optional storeFileOptions
    <*> Opt.optional databaseOutputOptions
    <*> Opt.switch
        ( Opt.long "bootstrap"
        <> Opt.help "Print config file and exit"
        )
  where
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

-- | Top level commands.
data TopCommand
    = TopFile FilePath
    | TopCmd CmdLine
    | TopDump FilePath
    | TopPrepare Db.Db
    deriving (Eq, Show)

topCommandOpts :: Opt.Parser TopCommand
topCommandOpts =
    ( C.subparserCmd "config ..." $ Opt.command "config" $ Opt.info
        (fmap TopFile parseConfigFile <**> Opt.helper)
        (Opt.progDesc "Use config file")
    )
   <|> ( C.subparserCmd "args ..." $ Opt.command "args" $ Opt.info
        (fmap TopCmd cmdLineOpts <**> Opt.helper)
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
 where
    parseConfigFile :: Opt.Parser FilePath
    parseConfigFile = Opt.argument Opt.str (Opt.metavar "FILE")

-- | Speciffic command options.
data Options = Options
    { optIdent      :: Maybe Event.SourceId
    , optUptime     :: Maybe Double
    , optStdin      :: Maybe Event.Channel
    , optStdout     :: Bool
    , optHttp       :: Maybe (Ip, Port)
    , optArgs       :: TopCommand
    } deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> Opt.optional Event.sourceIdOptions
    <*> Opt.optional ( Opt.option Opt.auto
        ( Opt.long "logUptime"
       <> Opt.help "Log uptime every N seconds"
       <> Opt.metavar "N"
        ))
    <*> Opt.optional stdinOption
    <*> Opt.switch
        (Opt.long "stdout"
       <> Opt.help "Enable stdoutput"
        )
    <*> Opt.optional httpOptions
    <*> topCommandOpts

  where

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

    stdinOption :: Opt.Parser Event.Channel
    stdinOption = Event.Channel <$> Opt.strOption
        ( Opt.long "stdin"
       <> Opt.metavar "CH"
       <> Opt.help (Prelude.unwords
            [ "Record events (lines) from stdin to given channel,"
            , "ignore other configured sources"
            ])
        )

emptyConfig :: AppConfig
emptyConfig = AppConfig
    { appInputs = Inputs mempty
    , appFileOutput = Nothing
    , appDatabaseOutput = Nothing
    }

-- | Custom version of 'drain', where each element is
-- fully evaluated, to prevent a leak.
drain :: (NFData a, Monad m) => Consumer a m r
drain = await >>= \x -> x `deepseq` drain

stdInInput :: (PS.MonadSafe m) =>
    Event.SourceId -> Event.SessionId -> Event.Channel
    -> Producer Event.Event m ()
stdInInput recId sessionId ch = do
    trackId <- liftIO (Event.trackId . Data.UUID.toString <$> nextRandom)
    liftIO $ logM INFO $ "ch: " ++ show ch ++ " -> trackId: " ++ show trackId
    File.fileReaderLines (File.FileStore "-") >-> go trackId minBound
  where
    go trackId sn = do
        s <- Event.Payload <$> await
        (tUtc, tMono) <- liftIO Event.now
        yield $ Event.Event ch recId tUtc tMono sessionId trackId sn s
        go trackId (Event.nextSequenceNum sn)

stdOutOutput :: (MonadIO m) => Consumer Event.Event m ()
stdOutOutput = PP.map (Enc.encode Enc.EncText) >-> PBS.stdout

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
        logM INFO $ "Start input: ch: " ++ show ch ++ ", " ++ show param
        a <- Ex.mask $ \restore -> do
            a <- Async.async $ restore $ autoRestart ch param
            atomically $ modifyTVar' active $ Set.insert a
            return a
        return (a, param)

    onStop active ch (a, param) = do
        logM INFO $ "Stop input: ch: " ++ show ch ++ ", " ++ show param
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
            logM INFO $
                "Auto restart input: ch: " ++ show ch ++ ", " ++ show param

-- | Single UDP input producer.
udpInput :: Event.SourceId -> Event.SessionId -> (Event.Channel, Udp.UdpIn)
    -> Producer Event.Event (PS.SafeT IO) ()
udpInput recId sessionId (ch, addr) = do
    trackId <- liftIO (Event.trackId . Data.UUID.toString <$> nextRandom)
    liftIO $ logM INFO $ "ch: " ++ show ch ++ " -> trackId: " ++ show trackId
    Udp.udpReader addr >-> PP.map fst >-> go trackId minBound
  where
    go trackId sn = do
        s <- Event.Payload <$> await
        (tUtc, tMono) <- liftIO Event.now
        yield $ Event.Event ch recId tUtc tMono sessionId trackId sn s
        go trackId (Event.nextSequenceNum sn)

-- | Merged all configured UDP inputs into a single producer.
udpInputs :: STM Inputs -> Event.SourceId -> Event.SessionId
    -> Producer Event.Event (PS.SafeT IO) ()
udpInputs getInputs recId sessionId = PS.bracket prepare finalize action
  where
    prepare = do
        (output, input) <- PC.spawn PC.unbounded
        a <- C.linkAsync $ do
            processInputs getInputs $ \(ch, udp) -> do
                let p = udpInput recId sessionId (ch, udp)
                PS.runSafeT $ runEffect $ p >-> PC.toOutput output
            PC.performGC
            fail "main UDP input process terminated"
        return (input, a)
    finalize (_, a) = Async.cancel a
    action (input, _) = fix $ \loop -> do
        mMsg <- liftIO $ atomically $ PC.recv input
        case mMsg of
            Nothing -> fail "UDP source exhausted"
            Just msg -> do
                yield msg
                loop

-- | Conditional file output, depending on configuration.
fileOutput :: STM (Maybe FileOutput) -> Consumer Event.Event (PS.SafeT IO) ()
fileOutput getCfg = C.consumeWith "file output" getCfg $ \case
    Nothing -> drain
    Just fo ->
        PP.map (Enc.encode $ outFileEnc fo)
        >-> File.rotatingFileWriter (open fo) (outFileRotate fo) (logM INFO)
  where
    open f = case outFileStore f of
        File.FileStore "-" -> File.streamStdout
        File.FileStore fs -> File.streamHandle fs

-- | Conditional database output, depending on configuration.
databaseOutput :: Event.SessionId -> (Db.Status -> STM ())
    -> STM (Maybe DatabaseOutput) -> Consumer Event.Event (PS.SafeT IO) ()
databaseOutput sessionId setStat getCfg =
  C.consumeWith "database output" getCfg $ \case
    Nothing -> drain
    Just dbo -> PS.bracket (prepare dbo) finalize action
 where
    prepare dbo = do
        (output, input) <- PC.spawn $ PC.bounded 1
        qOfw <- liftIO $ newTVarIO DS.empty -- overflow queue
        let dropEvents s = modifyTVar qOfw (|> s)
            db = outDatabase dbo
            onOfw = outDatabaseOnOverflow dbo
            bufSend = outDatabaseBufferSend dbo
            bufDrop = outDatabaseBufferDrop dbo

        -- drop handler
        a <- C.linkAsync $ do
            let producer = forever $ do
                    msgs <- liftIO $ atomically $ do
                        msgs <- readTVar qOfw
                        case DS.null msgs of
                            True -> retry
                            False -> do
                                writeTVar qOfw DS.empty
                                return msgs
                    mapM_ yield msgs
                (Event.SessionId sid) = sessionId
                consumer = case onOfw of
                    OnOverflowDrop -> drain
                    OnOverflowFile fo -> do
                        let fs1 = outFileStore fo
                            fs2 = File.filePath fs1 ++ "-" ++ sid
                            open = case fs1 of
                                File.FileStore "-" -> File.streamStdout
                                File.FileStore _ -> File.streamPath fs2
                        PP.map (Enc.encode $ outFileEnc fo) >->
                            File.rotatingFileWriter
                                open (outFileRotate fo) (logM INFO)
            PS.runSafeT $ runEffect $ producer >-> consumer
            PC.performGC
            fail "database drop handler terimnated"

        -- database writer
        b <- C.linkAsync $ do
            PS.runSafeT $ runEffect $ PC.fromInput input >->
                Db.databaseWriterProcess setStat bufSend bufDrop dropEvents db
            PC.performGC
            fail "database writer terimnated"

        return (output, a, b)

    finalize (_, a, b) = do
        Async.cancel b
        Async.cancel a

    action (output, _, _) = do
        PC.toOutput output

logUptime :: Event.MonoTime -> Double -> IO b
logUptime startTimeMono t = forever $ do
    C.threadDelaySec t
    (_t1, t2) <- Event.now
    let uptimeSec =
            Event.monoTimeToSeconds t2 - Event.monoTimeToSeconds startTimeMono
        uptimeDays = uptimeSec / (24*3600)
    logM INFO $
        "uptime: " ++ showFFloat (Just 3) uptimeDays "" ++ " days"

httpServer :: Event.UtcTime -> Event.MonoTime -> TVar (Maybe Db.Status)
    -> (String, Warp.Port) -> IO ()
httpServer startTimeUtc startTimeMono dbStatV (ip, port) = do
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
        hostname <- Event.sourceId . Text.pack <$> getHostName
        return $ fromMaybe hostname $ optIdent opts
    logM INFO $ "recorder: " ++ show recId

    sessionId <- Event.sessionId . Data.UUID.toString <$> nextRandom
    logM INFO $ "session: " ++ show sessionId

    -- Global configuration, either from file or command line.
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
                    { appInputs = optInputs args
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
            input = maybe
                (udpInputs (appInputs <$> readTVar cfg) recId sessionId)
                (stdInInput recId sessionId)
                (optStdin opts)

        runAll
            [ process_ "uptime" $
                maybe doNothing (logUptime startTimeMono) (optUptime opts)
            , process_ "http" $ maybe doNothing
                (httpServer startTimeUtc startTimeMono dbStat)
                (optHttp opts)
            , process_ "data flow" $ PS.runSafeT $ runEffect $
                input               -- inputs
                >-> C.forkConsumers -- outputs
                    [ bool drain stdOutOutput (optStdout opts)
                    , (fileOutput (appFileOutput <$> readTVar cfg))
                        >> liftIO (logM ERROR "file output terminated")
                    , (databaseOutput sessionId updateDbStatus
                        (appDatabaseOutput <$> readTVar cfg))
                        >> liftIO (logM ERROR "database output terminated")
                    ]
            ]

cmdRecord :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdRecord = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event recorder")

