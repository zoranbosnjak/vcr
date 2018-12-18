------------------
-- |
--
-- 'capture' command
--
-- Configuration options:
--  - configuration from file
--  - configuration from zookeeper
--      In this case, the file configuration will be automatically
--      overwritten on each change from zookeeper.
--      It makes sense to put dynamic config file to /var/run/...
--
-- Save data to (one or both):
--  - rotating file
--  - kafka cluster
--
-- TODO: - IPv6 support on unicast/multicast input "[ipv6]", "ip" syntax

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdCapture where

-- standard imports
import           GHC.Generics (Generic)
import           Options.Applicative
import           System.Log.Logger (Priority(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Text as Text
import           Data.String (fromString)
import           Data.Aeson
import           Data.Bool
import           Network.BSD (getHostName)
import           Data.Maybe (fromMaybe)
import qualified Data.UUID
import           Data.UUID.V4 (nextRandom)
import           Control.Monad
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Concurrent (threadDelay)
import           Control.Monad.Fix
import           Numeric (showFFloat)
import qualified System.Clock as Clk
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Time (UTCTime)
import           System.Posix.Files (touchFile)
import qualified Control.Exception as Ex
import           Pipes
import qualified Pipes.Safe as PS
import qualified Pipes.Prelude as PP
import qualified Database.Zookeeper as ZK
import           Kafka.Producer

-- local imports
import           Types
import           Config
import qualified Common as C
import           Common (logM)
import           Process
import qualified Encodings as Enc
import qualified Udp
import qualified Event
import qualified File

type KafkaUpdate = Either String UTCTime

data DstFile = DstFile File.FileStore Enc.EncodeFormat (Maybe File.Rotate)
    deriving (Generic, Eq, Show)

data DstKafka = DstKafka [BrokerAddress]
    deriving (Generic, Eq, Show)

data ZkConnection = ZkConnection ZkEndpoint ZK.Timeout
    deriving (Generic, Eq, Show)

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { optIdent      :: Maybe SourceId
    , optConfigFile :: Maybe FilePath
    , optZookeeper  :: Maybe ZkConnection
    , optDstFile    :: Maybe DstFile
    , optDstKafka   :: Maybe DstKafka
    , optLogUptime  :: Maybe Double
    , optHttp       :: Maybe (String, Int)
    } deriving (Generic, Eq, Show)

dstFile :: Parser DstFile
dstFile = C.subparserCmd "file ..." $
    command "file" $ info
        (opts <**> helper)
        (progDesc "Store data to a (rotating) file")
  where
    opts = DstFile
        <$> File.fileStoreOptions
        <*> Enc.encodeFormatOptions
        <*> optional File.rotateOptions

dstKafka :: Parser DstKafka
dstKafka = C.subparserCmd "kafka ..." $
    command "kafka" $ info
        (opts <**> helper)
        (progDesc "Store data to a kafka cluster")
  where
    opts = DstKafka
        <$> some ( BrokerAddress <$> strOption
            ( long "kafka"
           <> help "Broker address, eg: localhost:9092"
            ))

-- | Command option parser.
options :: Parser CmdOptions
options = CmdOptions
    <$> optional ( strOption
        ( long "ident"
       <> metavar "IDENT"
       <> help "Recorder identifier"
        ))
    <*> optional ( strOption
        ( long "config"
       <> metavar "FILE"
       <> help "config file"
        ))
    <*> optional ( ZkConnection
        <$> strOption
            ( long "zookeeper"
           <> metavar "ZK"
           <> help "zookeeper endpoint, eg: localhost:2181,localhost:2182/foobar"
            )
        <*> option auto
            ( long "zookeeperTimeout"
           <> metavar "ms"
           <> help "zookeeper connection timeout in miliseconds"
           <> value 6000
            )
        )
    <*> optional dstFile
    <*> optional dstKafka
    <*> optional ( option auto
        ( long "logUptime"
       <> help "Log uptime every N seconds"
       <> metavar "N"
        ))
    <*> optional httpOptions

  where

    httpOptions :: Parser (String, Int)
    httpOptions = C.subparserCmd "http ..." $
        command "http" $ info
            (opts <**> helper)
            (progDesc "Enable http server")
      where
        opts = (,)
            <$> parseIp
            <*> parsePort
        parseIp = strOption
            ( long "ip"
           <> metavar "IP"
           <> help "Ip address"
           <> value "127.0.0.1"
           <> showDefault
            )
        parsePort = option auto
            ( long "port"
           <> metavar "PORT"
           <> help "TCP port number"
           <> value 8080
           <> showDefault
            )

-- | Process each input with 'act'.
processInputs :: STM Inputs -> ((Channel, Either String UdpIn) -> IO a) -> IO b
processInputs getInputs act = do
    active <- newTVarIO mempty
    loop active mempty `Ex.finally` terminate active
  where
    terminate active = do
        s <- atomically $ readTVar active
        mapM_ cancel s

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
            a <- async $ restore $ autoRestart ch param
            atomically $ modifyTVar' active $ Set.insert a
            return a
        return (a, param)

    onStop active ch (a, param) = do
        logM INFO $ "Stop input: ch: " ++ show ch ++ ", " ++ show param
        Ex.mask $ \_restore -> do
            atomically $ modifyTVar' active $ Set.delete a
            cancel a

    onRestart active ch (running, newParam) = do
        onStop active ch running
        onStart active ch newParam

    autoRestart ch param = forever $
        withAsync (act (ch, param)) $ \a -> do
            rv <- waitCatch a
            let msg = case rv of
                    Left e -> "with exception: " ++ show e
                    Right _ -> "without exception"
            logM NOTICE $ "Input terminated: "
                ++ show ch ++ ", " ++ show param ++ ", " ++ msg
                ++ ", will auto restart in few moments..."
            C.threadDelaySec 3
            logM INFO $
                "Auto restart input: ch: " ++ show ch ++ ", " ++ show param

-- | Single UDP input handler.
udpInput :: SourceId -> SessionId -> Consumer Event.Event (PS.SafeT IO) ()
    -> (Channel, Either [Char] UdpIn) -> IO ()
udpInput recId sesId dst (ch, eAddr) = case eAddr of
    Left e -> do
        logM INFO $ "ch: " ++ show ch ++ "failure: " ++ e
        doNothing
    Right addr -> do
        trackId <- Data.UUID.toText <$> nextRandom
        logM INFO $ "ch: " ++ show ch ++ " -> trackId: " ++ show trackId
        PS.runSafeT $ runEffect $
            Udp.udpReader addr
            >-> PP.map fst
            >-> go trackId 0
            >-> dst
  where
    go trackId !sn = do
        s <- await
        (tMono, tUtc) <- liftIO C.now
        yield $ Event.Event ch recId tUtc tMono sesId trackId sn s
        go trackId (Event.nextSequenceNum sn)

-- | Get uptime (in days)
getUptimeDays :: Clk.TimeSpec -> IO Double
getUptimeDays start = do
    t <- C.nowMono
    let uptimeSec = C.monoTimeToSeconds t - C.monoTimeToSeconds start
        uptimeDays = (uptimeSec :: Double) / (24*3600)
    return uptimeDays

-- | log uptime
logUptime :: Clk.TimeSpec -> Double -> IO b
logUptime start d = forever $ do
    C.threadDelaySec d
    val <- getUptimeDays start
    logM INFO $ "uptime: " ++ showFFloat (Just 3) val "" ++ " days"

-- | HTTP server for simple process monitoring.
httpServer :: UTCTime -> Clk.TimeSpec -> SessionId
    -> TVar BS.ByteString
    -> TVar (Maybe String)
    -> TVar Inputs
    -> TVar (Maybe String)
    -> TVar KafkaUpdate
    -> (String, Warp.Port)
    -> IO ()
httpServer startTimeUtc startTimeMono sesId rawConfig configError inputs zkError kfkStatus (ip,port) = do
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
            t <- getUptimeDays startTimeMono
            let uptimeDaysStr = showFFloat (Just 3) t "" ++ " days\n"
            respond $ responseLBS status200 [("Content-Type", "text/plain")]
                (BSL8.pack $ uptimeDaysStr)

        go ["session"] GET = respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (BSL8.pack $ show sesId ++ "\n")

        go ["config"] GET = do
            val <- atomically $ readTVar rawConfig
            respond $ responseLBS status200 [("Content-Type", "text/plain")]
                (BSL8.fromStrict val <> "\n")

        go ["inputs"] GET = do
            val <- atomically $ readTVar inputs
            respond $ responseLBS status200 [("Content-Type", "text/plain")]
                (Enc.jsonEncodePretty val <> "\n")

        go ["status"] GET = do
            configError' <- atomically $ readTVar configError
            zkError' <- atomically $ readTVar zkError
            kfkStatus' <- atomically $ readTVar kfkStatus
            let stat = object
                    [ "config error"    .= configError'
                    , "zookeeper error" .= zkError'
                    , "kakfa status"    .= show kfkStatus'
                    ]
            respond $ responseLBS status200 [("Content-Type", "text/plain")]
                (Enc.jsonEncodePretty stat <> "\n")

        -- not found
        go _ _ = respond $ responseLBS
            status404
            [("Content-Type", "text/plain")]
            "404 - Not Found\n"

-- | Converter from raw input to (config error, inputs).
configMonitor :: SourceId -> TVar BS.ByteString -> TVar (Maybe String)
    -> TVar Inputs -> IO b
configMonitor recId rawConfig configError inputs = do
    (atomically $ readTVar rawConfig) >>= loop
  where
    loop raw = do
        case inputConfig recId raw of
            Left e -> atomically $ writeTVar configError $ Just e
            Right val -> atomically $ do
                writeTVar configError Nothing
                writeTVar inputs val
        y <- atomically $ do
            y <- readTVar rawConfig
            bool (return y) retry (y == raw)
        loop y

-- Auto write any change from 'rawConfig' to a file,
-- assume initial file is up-to-date.
monFile :: TVar BS.ByteString -> FilePath -> IO b
monFile rawConfig f = (atomically $ readTVar rawConfig) >>= loop where
    loop x = do
        y <- atomically $ do
            y <- readTVar rawConfig
            bool (return y) retry (x == y)
        logM INFO $ "config changed, writing to " ++ show f
        BS.writeFile f y
        loop y

-- periodically update from zookeeper to 'rawConfig'
monZk :: TVar (Maybe String) -> TVar BS.ByteString -> ZkConnection -> IO a
monZk zkError rawConfig (ZkConnection zkEndpoint zkTimeout) = do
    ZK.withZookeeper zkEndpoint zkTimeout Nothing Nothing $ \zk -> forever $ do
        rv <- race (threadDelay (zkTimeout*1000)) $ fix $ \loop -> do
            ZK.get zk "/inputs" Nothing >>= \case
                Left e -> do
                    atomically $ writeTVar zkError $ Just $ "ZKError: " ++ show e
                    loop
                Right (Nothing,_) -> do
                    atomically $ writeTVar zkError $ Just "No content"
                    loop
                Right (Just val,_) -> do
                    return val
        case rv of
            Left _ -> do
                atomically $ writeTVar zkError $ Just "ZK timeout"
            Right s -> atomically $ do
                writeTVar zkError Nothing
                writeTVar rawConfig s
        C.threadDelaySec 1

-- | Send data to rotating file.
toFile :: TChan Event.Event -> DstFile -> IO ()
toFile q (DstFile fs enc mRot) = do
    src <- atomically $ dupTChan q
    let fromQueue = forever ((liftIO $ atomically $ readTChan src) >>= yield)
    PS.runSafeT $ runEffect $
        fromQueue
        >-> PP.map (Enc.encode enc)
        >-> File.rotatingFileWriter open mRot (logM INFO)
  where
    open = case fs of
        File.FileStore "-" -> File.streamStdout
        File.FileStore f -> File.streamHandle f

-- | Send data to kafka cluster.
toKafka :: TChan Event.Event -> (Either String UTCTime -> STM ()) -> DstKafka -> IO ()
toKafka q setStatus (DstKafka kafka) = do
    src <- atomically $ dupTChan q
    let fromQueue = forever ((liftIO $ atomically $ readTChan src) >>= yield)
    PS.runSafeT $ runEffect $
        fromQueue
        >-> PS.bracket prepare closeProducer go
  where
    prepare = newProducer producerProps >>= \case
        Left e -> fail $ show e
        Right prod -> return prod

    go prod = forever $ do
        event <- await
        rv <- liftIO $ produceMessage prod (mkMessage event)
        case rv of
            Nothing -> return ()
            Just e -> liftIO $ logM NOTICE $ show e

    producerProps :: ProducerProperties
    producerProps = brokersList kafka
         <> sendTimeout (Timeout 10000)
         <> setCallback (deliveryCallback onDelivery)

    mkMessage :: Event.Event -> ProducerRecord
    mkMessage event = ProducerRecord
        { prTopic = TopicName $ Event.eChannel event
        , prPartition = UnassignedPartition
        , prKey = Nothing
        , prValue = Just $ BSL.toStrict $ Data.Aeson.encode event
        }

    onDelivery dr = do
        t <- C.nowUtc
        atomically $ setStatus $ case dr of
            DeliverySuccess _ _ -> Right t
            DeliveryFailure _ e -> Left $ "failure: " ++ show e
            NoMessageError e -> Left $ "no message: " ++ show e

-- | Run command.
runCmd :: CmdOptions -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    (startTimeMono, startTimeUtc) <- C.now
    logM INFO $ "startup " ++ show startTimeUtc
    logM INFO $
        "command 'record', opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    recId <- do
        hostname <- Text.pack <$> getHostName
        return $ fromMaybe hostname $ optIdent opts
    logM INFO $ "recorder: " ++ show recId

    sesId <- Data.UUID.toText <$> nextRandom
    logM INFO $ "session: " ++ show sesId

    rawConfig <- newTVarIO =<< case optConfigFile opts of
        Nothing -> return BS.empty
        Just f -> touchFile f >> BS.readFile f

    configError <- newTVarIO Nothing

    inputs <- newTVarIO (mempty :: Inputs)

    zkError <- newTVarIO Nothing

    kfkStatus <- newTVarIO $ Right startTimeUtc

    q <- newBroadcastTChanIO
    let toQueue :: Consumer Event.Event (PS.SafeT IO) c
        toQueue = PP.mapM_ (liftIO . atomically . writeTChan q)

    runAll
        [ process_ "uptime" $
            runMaybe (optLogUptime opts) (logUptime startTimeMono)

        , process_ "config -> inputs" $
            configMonitor recId rawConfig configError inputs

        , process_ "config file monitor" $      -- write config to a file
            runMaybe (optConfigFile opts) (monFile rawConfig)

        , process_ "zookeeper monitor" $        -- read config from zookeeper
            runMaybe (optZookeeper opts) (monZk zkError rawConfig)

        , process_ "http" $ maybe doNothing
            (httpServer startTimeUtc startTimeMono sesId rawConfig
                configError inputs zkError kfkStatus)
            (optHttp opts)

        , process_ "data capture" $
            processInputs (readTVar inputs) $ udpInput recId sesId toQueue

        , process_ "file writer" $
            runMaybe (optDstFile opts) $ toFile q

        , process_ "kafka writer" $
            runMaybe (optDstKafka opts) $ \dst -> do
                ZK.setDebugLevel ZK.ZLogError
                let setStatus = writeTVar kfkStatus
                toKafka q setStatus dst
        ]
  where
    runMaybe = flip (maybe doNothing)

-- | toplevel command
cmdCapture :: ParserInfo (C.VcrOptions -> IO ())
cmdCapture = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Event recorder")

