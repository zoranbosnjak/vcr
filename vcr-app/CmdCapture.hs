
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdCapture where

-- standard imports
import           Control.Monad
import           Control.Monad.Fix
import           Control.Exception (mask, try, finally, IOException)
import           GHC.Generics (Generic)
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Options.Applicative
import qualified Data.UUID
import           Data.UUID.V4 (nextRandom)
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Socket as Net
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Types
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AesonP
import           System.Posix.Signals
import           Pipes
import qualified Pipes.Safe as PS

-- local imports
import           Common
import           Vcr
import           Time
import           Sequential
import           Udp
import           File

data Config = Config
    { confInputs :: Map.Map Channel UdpIn
    , confOutputFile :: Maybe (FilePath, Maybe Rotate)
    --, confOutputKafka :: Maybe ...
    } deriving (Generic, Eq, Show, ToJSON, FromJSON)

emptyConfig :: Config
emptyConfig = Config
    { confInputs = mempty
    , confOutputFile = Nothing
    }

type Bootstrap = Bool

data HttpConfig = HttpConfigEnabled | HttpConfigDisabled
    deriving (Generic, Eq, Show)

data SigHUPConfig = SigHUPConfigEnabled | SigHUPConfigDisabled
    deriving (Generic, Eq, Show)

data ConfigMethod
    = ConfigArguments Config Bootstrap
    | ConfigFile FilePath HttpConfig SigHUPConfig
    -- | ConfigZookeeper FilePath ZkConnection
        --data ZkConnection = ZkConnection ZkEndpoint ZK.Timeout deriving (Generic, Eq, Show)
    deriving (Generic, Eq, Show)

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { optVerbose    :: Maybe Priority
    , optSyslog     :: Maybe Priority
    , optHttp       :: Maybe (String, Warp.Port)
    , optConfig     :: ConfigMethod
    , optAlarmHold  :: Double
    } deriving (Generic, Eq, Show)

options :: Parser CmdOptions
options = CmdOptions
    <$> optional (option auto
        ( short 'v' <> long "verbose" <> metavar "LEVEL"
       <> help ("Set console verbosity level, one of: " ++ show levels)))
    <*> optional (option auto
        ( long "syslog" <> metavar "LEVEL"
       <> help ("Set syslog verbosity level, one of: " ++ show levels)))
    <*> optional httpOptions
    <*> configMethod
    <*> option auto
        ( long "alarmHold"
       <> metavar "SEC"
       <> help "Hold last alarm (NOTICE) for number of seconds"
       <> showDefault
       <> value 60.0
        )
  where
    levels = [minBound..maxBound] :: [Priority]
    httpOptions = (,)
        <$> strOption (long "http" <> metavar "IP")
        <*> option auto (long "httpPort" <> metavar "PORT")
    configMethod
        = (flag' () (long "arguments" <> help "use command line arguments")) *> confArguments
        <|> (flag' () (long "file" <> help "use config file")) *> confFile
      where
        confArguments = ConfigArguments
            <$> conf
            <*> switch (long "bootstrap" <> help "dump configuration")
          where
            conf :: Parser Config
            conf = Config
                <$> (Map.fromList <$> many
                    ((,) <$> channelOption <*> (inUnicast <|> inMulticast)))
                <*> optional fileOutOptions
              where
                channelOption = strOption
                    (long "channel" <> metavar "CH" <> help "channel identifier")
                fileOutOptions = (,)
                    <$> strOption (long "fileOutput" <> metavar "FILE" <> help "base filename")
                    <*> optional ( Rotate
                        <$> option auto (long "rotateKeep" <> help "Keep number of rotated files")
                        <*> optional (option auto (long "mega" <> help "max file size"))
                        <*> optional (option auto (long "hours" <> help "max file age"))
                        )
                inUnicast = UdpInUnicast
                    <$> strOption (long "unicast" <> metavar "IP")
                    <*> strOption (long "port" <> metavar "PORT")
                inMulticast = UdpInMulticast
                    <$> strOption (long "multicast" <> metavar "IP")
                    <*> strOption (long "port" <> metavar "PORT")
                    <*> optional (strOption (long "local" <> metavar "IP"))
        confFile = ConfigFile
            <$> strOption (long "path" <> metavar "FILE" <> help "config file")
            <*> flag HttpConfigDisabled HttpConfigEnabled (long "enableHttpConfig")
            <*> flag SigHUPConfigDisabled SigHUPConfigEnabled (long "enableSigHUPConfig")

-- | Encode (pretty) to JSON.
encodePretty :: (Data.Aeson.ToJSON a) => a -> BSL.ByteString
encodePretty = AesonP.encodePretty'
    AesonP.defConfig {AesonP.confCompare = compare}

httpServer ::
    (Priority -> String -> IO ())
    -> MonoTimeNs
    -> UtcTime
    -> SessionId
    -> TVar Config
    -> Alarm (Priority, String, String)
    -> ConfigMethod
    -> (String, Warp.Port)
    -> IO ()
httpServer logM startTimeMono startTimeUtc sesId config logAlarms cfgMethod (ip, port) = do
    let settings =
            Warp.setPort port $
            Warp.setHost (fromString ip) $
            Warp.defaultSettings
    logM INFO $ "http server, ip: " ++ show ip ++ ", port: " ++ show port
    Warp.runSettings settings $ app
  where
    app request respond = withLog go
      where
        jsonEncFormat :: Data.Aeson.ToJSON a => a -> BSL.ByteString
        jsonEncFormat = case "pretty" `elem` (fst <$> queryString request) of
            False -> encode
            True -> encodePretty

        withLog act = case parseMethod (requestMethod request) of
            Left _ -> respond $ responseLBS
                status400
                [("Content-Type", "text/plain")]
                "400 - unknown request method\n"
            Right mtd -> do
                let level = case mtd of
                        GET -> DEBUG
                        _ -> INFO
                logM level $ show request
                act (pathInfo request) mtd

        go ["ping"] GET = respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (BSL8.pack "pong\n")

        go ["startTime"] GET = respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (BSL8.pack $ show startTimeUtc ++ "\n")

        go ["uptime"] GET = do
            uptime <- uptimeDaysStr startTimeMono <$> getMonoTimeNs
            respond $ responseLBS status200 [("Content-Type", "text/plain")]
                (BSL8.pack $ uptime ++ "\n")

        go ["session"] GET = respond $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            (BSL8.pack $ show sesId ++ "\n")

        go ["config"] GET = do
            cfg <- atomically $ readTVar config
            respond $ responseLBS status200
                [("Content-Type", "application/json")]
                (jsonEncFormat cfg)

        go ["config"] PUT = case cfgMethod of
            ConfigFile path HttpConfigEnabled _sigHupFlag -> do
                (eitherDecode <$> strictRequestBody request) >>= \case
                    Left e -> respond $ responseLBS status400
                        [("Content-Type", "text/plain")] (BSL8.pack $ e ++ "\n")
                    Right cfg -> do
                        oldCfg <- atomically $ swapTVar config cfg
                        when (cfg /= oldCfg) $ do
                            logM INFO $ "New configuration received: " ++ show cfg
                            logM INFO $ "Writting configuration to: " ++ show path
                            BSL.writeFile path $  encodePretty cfg
                        respond $ responseLBS
                            status200
                            [("Content-Type", "text/plain")]
                            "ok"
            _ -> notFound

        go ["status"] GET = do
            alm <- atomically $ do
                getAlarm logAlarms
            let logError = case alm of
                    Nothing -> Null
                    Just (prio, name, msg) -> object
                        [ ("priority", String $ T.pack $ show prio)
                        , ("module", String $ T.pack name)
                        , ("message", String $ T.pack msg)
                        ]
                status = object
                    [ ("log error", logError)
                    ]
            respond $ responseLBS status200
                [("Content-Type", "application/json")]
                (jsonEncFormat status)

        go _ _ = notFound

        notFound = respond $ responseLBS
            status404
            [("Content-Type", "text/plain")]
            "404 - Not Found\n"

-- | Single UDP input handler.
singleInput :: SessionId -> (Priority -> String -> IO ()) -> (UdpEvent -> STM ())
    -> Channel -> UdpIn -> IO ()
singleInput sesId logM consume ch i = do
    trackId <- Data.UUID.toText <$> nextRandom
    logM INFO $ "ch: " ++ show ch ++ ", " ++ show i ++ " -> trackId:" ++ show trackId
    let src = udpReader i
    forever $ do
        result <- try $ PS.runSafeT $ runEffect $
            src >-> mkEvent trackId firstSequence >-> dst
        let msg = case result of
                Left e -> "with exception: " ++ show (e :: IOException)
                Right _ -> "without exception"
        logM NOTICE $ "Input terminated: " ++ show i ++ ", " ++ msg
        threadDelaySec 3
        logM INFO $ "Auto restarting input " ++ show i
  where
    mkEvent trackId = fix $ \loop n -> do
        (datagram, sender) <- await
        timeMono <- liftIO getMonoTimeNs
        timeUtc  <- liftIO getUtcTime
        lookupResult <- liftIO $ Net.getNameInfo [Net.NI_NUMERICHOST, Net.NI_NUMERICSERV] True True sender
        case lookupResult of
            (Just senderHost, Just senderPort) -> do
                yield $ Event ch timeMono timeUtc sesId trackId n $ UdpContent datagram (senderHost, senderPort)
                loop $ nextSequence n
            _ -> do
                liftIO $ logM ERROR $ "ch: " ++ show ch ++ ", " ++ show i ++ " host/port lookup error"

    dst = forever $ await >>= liftIO . atomically . consume

-- Process configured inputs.
processInputs :: SessionId
    -> (Priority -> String -> IO ())
    -> (UdpEvent -> STM ())
    -> STM (Map.Map Channel UdpIn)
    -> IO ()
processInputs sesId logM consume getInputs = do
    logM INFO "starting process"
    active <- newTVarIO mempty
    loop active mempty `finally` terminate active
  where
    terminate active = atomically (readTVar active) >>= mapM_ cancel

    onStart active ch i = do
        logM INFO $ "Starting input, ch: " ++ show ch ++ ", " ++ show i
        mask $ \restore -> do
            a <- async $ restore $ singleInput sesId logM consume ch i
            link a
            atomically $ modifyTVar' active $ Map.insert i a

    onStop active ch i = do
        logM INFO $ "Stopping input, ch: " ++ show ch ++ ", " ++ show i
        mask $ \_restore -> do
            Just a <- atomically $ do
                orig <- readTVar active
                modifyTVar' active $ Map.delete i
                return $ Map.lookup i orig
            cancel a

    onRestart active ch (i,j) = do
        onStop active ch i
        onStart active ch j

    loop active current = do
        newCfg <- atomically $ do
            newCfg <- getInputs
            when (newCfg == current) retry
            return newCfg

        let removed = current `Map.difference` newCfg
            added = newCfg `Map.difference` current
            checkF k v = do
                oldParam <- Map.lookup k current
                newParam <- Map.lookup k newCfg
                guard $ newParam /= oldParam
                return (v, newParam)
            changed = Map.mapMaybeWithKey checkF current

        _ <- Map.traverseWithKey (onStop active) removed
        _ <- Map.traverseWithKey (onStart active) added
        _ <- Map.traverseWithKey (onRestart active) changed

        loop active newCfg

runCmd :: CmdOptions -> Prog -> Args -> Version -> GhcBase -> WxcLib -> IO ()
runCmd opt pName pArgs version _ghc _wxcLib = do
    startTimeMono <- getMonoTimeNs
    startTimeUtc <- getUtcTime

    -- last severe log event is held for some time
    logAlarms <- newAlarmIO (optAlarmHold opt)
    let log2Alarm prio name msg = when (prio >= NOTICE) $ do
            atomically $ refreshAlarm logAlarms (prio, name, msg)

    logM <- setupLogging pName "capture" (optVerbose opt) (optSyslog opt) (Just log2Alarm)

    logM "main" INFO $ "startup " ++ show pName ++ ", " ++ version ++ ", " ++ show pArgs
    logM "main" INFO $ show opt

    sesId <- Data.UUID.toText <$> nextRandom
    logM "main" INFO $ "session: " ++ show sesId

    config <- newTVarIO emptyConfig

    q <- newBroadcastTChanIO

    proceed <- case optConfig opt of
        ConfigArguments cfg True -> do
            BSL.putStr $ encodePretty cfg
            return False
        ConfigArguments cfg False -> do
            atomically $ writeTVar config cfg
            return True
        ConfigFile path _httpFlag sigHupFlag -> do
            let loadConfig = do
                    logM "main" INFO $ "Loading configuration from " ++ show path
                    result <- try $ do
                        s <- BSL.readFile path
                        case eitherDecode s of
                            Right val -> return val
                            Left e -> fail e
                    case result of
                        Left e -> do
                            let msg = "Error loading file: " ++ show (e :: IOException)
                            logM "main" NOTICE msg
                        Right cfg -> do
                            logM "main" INFO $ show cfg
                            atomically $ writeTVar config cfg
                reloadConfig = do
                    logM "main" INFO "SIGHUP received, reloading configuration."
                    loadConfig
            loadConfig
            when (sigHupFlag == SigHUPConfigEnabled) $ do
                void $ installHandler sigHUP (Catch reloadConfig) Nothing
            return True

    when proceed $ do
        ix <- runAll
            -- handle log alarms
            [ runAlarm logAlarms

            -- periodic uptime logging
            , periodic 600 $ do
                uptime <- uptimeDaysStr startTimeMono <$> getMonoTimeNs
                logM "main" INFO $ "uptime: " ++ uptime ++ ", " ++ version

            -- http server
            , httpServer (logM "main") startTimeMono startTimeUtc sesId config logAlarms (optConfig opt)
                `whenSpecified` (optHttp opt)

            -- send to recorder
            , do
                ch <- atomically $ dupTChan q
                let getConfig = confOutputFile <$> readTVar config
                    -- fetchLine = BSL.toStrict . encodeCompact <$> readTChan ch
                -- rotatingFileLineWriter (logM "fileWriter") getConfig fetchLine
                rotatingFileLineWriter (logM "fileWriter") getConfig (readTChan ch)

            -- input processing
            , processInputs sesId (logM "processInputs")
                (writeTChan q)
                (confInputs <$> readTVar config)
            ]

        logM "main" NOTICE $ "process terminated, index: " ++ show ix

-- | toplevel command
cmdCapture :: ParserInfo Command
cmdCapture = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Event recorder")

