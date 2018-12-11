------------------
-- |
--
-- 'capture' command
--
-- TODO: - IPv6 support on unicast/multicast input "[ipv6]", "ip" syntax

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Time (UTCTime)
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

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { optZk         :: String
    , optZkTimeout  :: ZK.Timeout
    , optKafka      :: [BrokerAddress]
    , optIdent      :: Maybe SourceId
    , optLogUptime  :: Maybe Double
    , optHttp       :: Maybe (String, Int)
    } deriving (Eq, Show)

-- | Zookeeper configuration.
data ZkConfig = ZkConfig
    { cfgInputs :: Inputs
    } deriving (Generic, Eq, Show)
instance ToJSON ZkConfig
instance FromJSON ZkConfig

-- | Command option parser.
options :: Parser CmdOptions
options = CmdOptions
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
    <*> some ( BrokerAddress <$> strOption
        ( long "kafka"
       <> help "Broker address, eg: localhost:9092"
        ))
    <*> optional ( strOption
        ( long "ident"
       <> metavar "IDENT"
       <> help "Recorder identifier"
        ))
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

-- | Send consumed data to kafka cluster.
toKafka :: [BrokerAddress] -> Consumer Event.Event (PS.SafeT IO) c
toKafka kafka = PS.bracket prepare closeProducer go
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

    mkMessage :: Event.Event -> ProducerRecord
    mkMessage event = ProducerRecord
        { prTopic = TopicName "test"
        , prPartition = UnassignedPartition
        , prKey = Nothing
        , prValue = Just $ BSL.toStrict $ Data.Aeson.encode event
        }

-- | Single UDP input handler.
udpInput :: [BrokerAddress] -> SourceId -> SessionId
    -> (Channel, Either String UdpIn) -> IO ()
udpInput kafka recId sesId (ch, eAddr) = case eAddr of
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
            >-> toKafka kafka
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
    -> TVar (Either String ZkConfig)
    -> (String, Warp.Port) -> IO ()
httpServer startTimeUtc startTimeMono sesId zkConfig (ip, port) = do
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

        go ["status"] GET = do
            zkConfig' <- atomically $ readTVar zkConfig
            let stat = object
                    [ "session" .= sesId
                    , "config"  .= zkConfig'
                    ]
            respond $ responseLBS status200 [("Content-Type", "text/plain")]
                (Enc.jsonEncodePretty stat <> "\n")

        -- not found
        go _ _ = respond $ responseLBS
            status404
            [("Content-Type", "text/plain")]
            "404 - Not Found\n"

-- | Start action when input value becomes 'Right', then update 'Right' values.
whenRight :: (Eq c) => (b -> c) -> TVar (Either a b) -> (TVar c -> IO d) -> IO d
whenRight getter x act = do
    initial <- atomically (fetch >>= either (const retry) return)
    var <- newTVarIO initial
    withAsync (monitor var initial) $ \_ -> act var
  where
    fetch = fmap getter <$> readTVar x
    monitor var prev = do
        current <- atomically $ fetch >>= \case
            Left _ -> retry
            Right val -> case val == prev of
                True -> retry
                False -> do
                    writeTVar var val
                    return val
        monitor var current

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

    let zkFailure = "Reading zookeeper config" :: String
    zkConfig <- newTVarIO $ Left zkFailure

    ZK.setDebugLevel ZK.ZLogError

    runAll
        [ process_ "uptime" $
            maybe doNothing (logUptime startTimeMono) (optLogUptime opts)

        , process_ "zkConfig" $ do
            let timeout = optZkTimeout opts
            ZK.withZookeeper (optZk opts) timeout Nothing Nothing $ \zk ->
                forever $ do
                    rv <- race (threadDelay (timeout*1000)) $ fix $ \loop -> do
                        ZK.get zk "/inputs" Nothing >>= \case
                            Left _ -> loop
                            Right (Nothing,_) -> loop
                            Right (Just val,_) -> return val
                    let cfg = case rv of
                            Left _ -> Left zkFailure
                            Right s -> ZkConfig <$> inputConfig recId s
                    atomically $ writeTVar zkConfig cfg
                    C.threadDelaySec 1

        , process_ "http" $ maybe doNothing
            (httpServer startTimeUtc startTimeMono sesId zkConfig)
            (optHttp opts)

        , process_ "data flow" $ whenRight cfgInputs zkConfig $ \i ->
            processInputs (readTVar i) $ udpInput (optKafka opts) recId sesId

        ]

-- | toplevel command
cmdCapture :: ParserInfo (C.VcrOptions -> IO ())
cmdCapture = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Event recorder")

