{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module CmdCapture where

-- standard imports
import           Control.Monad
import           Control.Monad.Fix
import           UnliftIO
import qualified Control.Concurrent.STM as STM
import           GHC.Generics (Generic)
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
import           System.Posix.Signals
import           Pipes
import qualified Pipes.Safe as PS

-- local imports
import           Logging
import           Common
import           Vcr
import           Time
import           Sequential
import           Udp
import           Streaming.Disk
import           Capture.Types

data InputProcess = InputProcess
    { inProcess :: Async ()
    , inStatus :: TVar (TVar Bool)
    }

type InputStatus = Map.Map Channel InputProcess

type Bootstrap = Bool

data HttpConfig = HttpConfigEnabled | HttpConfigDisabled
    deriving (Generic, Eq, Show)

data SigHUPConfig = SigHUPConfigEnabled | SigHUPConfigDisabled
    deriving (Generic, Eq, Show)

data ConfigMethod
    = ConfigArguments Config Bootstrap
    | ConfigFile FilePath HttpConfig SigHUPConfig
    deriving (Generic, Eq, Show)

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { optVerbose    :: Maybe Priority
    , optSyslog     :: Maybe Priority
    , optHttp       :: Maybe (String, Warp.Port)
    , optConfig     :: ConfigMethod
    } deriving (Generic, Eq, Show)

-- | Option parser.
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
  where
    levels = [minBound..maxBound] :: [Priority]
    httpOptions = (,)
        <$> strOption (long "http" <> metavar "IP" <> help "enable http server")
        <*> option auto (long "httpPort" <> metavar "PORT" <> help "http server port")
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
                    <*> ( Rotate
                        <$> optional (option auto (long "rotateKeep" <> help "Keep number of rotated files"))
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
            <*> flag HttpConfigDisabled HttpConfigEnabled
                (long "enableHttpConfig" <> help "enable http reconfiguration")
            <*> flag SigHUPConfigDisabled SigHUPConfigEnabled
                (long "enableSigHUPConfig" <> help "enable SIGHUP reconfiguration")

-- | Http server.
httpServer ::
    (Priority -> String -> IO ())
    -> MonoTimeNs
    -> UtcTime
    -> SessionId
    -> TVar Config
    -> STM InputStatus
    -> ConfigMethod
    -> (String, Warp.Port)
    -> IO ()
httpServer logM startTimeMono startTimeUtc sesId config getStatus cfgMethod (ip, port) = do
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
            True -> encodeJSONPrettyL

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
                            BSL.writeFile path $ encodeJSONPrettyL cfg
                        respond $ responseLBS
                            status200
                            [("Content-Type", "text/plain")]
                            "ok"
            _ -> notFound

        go ["status"] GET = do
            status <- atomically $ do
                t <- fmap (Map.map inStatus) getStatus
                mapM (readTVar >=> readTVar) t
            respond $ responseLBS status200
                [("Content-Type", "application/json")]
                (jsonEncFormat (status :: Map.Map Channel Bool))

        go _ _ = notFound

        notFound = respond $ responseLBS
            status404
            [("Content-Type", "text/plain")]
            "404 - Not Found\n"

-- | Single UDP input handler.
singleInput ::
    SessionId
    -> (Priority -> String -> IO ())
    -> (Event UdpContent -> STM ())
    -> Channel
    -> UdpIn
    -> IO InputProcess
singleInput sesId logM consume ch i = do
    status <- newTVarIO True >>= newTVarIO
    InputProcess
        <$> async (runInput status)
        <*> pure status
  where
    mkEvent trackId = fix $ \loop n -> do
        (datagram, sender) <- await
        timeMono <- liftIO getMonoTimeNs
        timeUtc  <- liftIO getUtcTime
        lookupResult <- liftIO $ Net.getNameInfo [Net.NI_NUMERICHOST, Net.NI_NUMERICSERV] True True sender
        case lookupResult of
            (Just senderHost, Just senderPort) -> do
                yield $ Event ch timeMono timeUtc sesId trackId n $ UdpContent datagram (senderHost, senderPort)
                loop $! nextSequence n
            _ -> do
                liftIO $ logM ERROR $ "ch: " ++ show ch ++ ", " ++ show i ++ " host/port lookup error"

    dst = forever $ await >>= liftIO . atomically . consume

    runInput status = forever $ do
        let retryTimeoutSec = 3.0

        trackId <- Data.UUID.toText <$> nextRandom
        logM INFO $ "ch: " ++ show ch ++ ", " ++ show i ++ " -> trackId:" ++ show trackId
        let src = udpReader i

        result <- tryIO $ PS.runSafeT $ runEffect $
            src >-> mkEvent trackId (firstSequence :: SequenceNumber) >-> dst
        let msg = case result of
                Left e -> "with exception: " ++ show e
                Right _ -> "without exception"
        logM NOTICE $ "Input terminated: " ++ show i ++ ", " ++ msg
        -- Status will become 'True' evenutally, when stable long enough,
        -- that is: no more need for auto restart.
        -- The network interface might become alive for example.
        registerDelay (round $ retryTimeoutSec * 1.5 * 1000 * 1000)
            >>= atomically . writeTVar status
        threadDelaySec retryTimeoutSec
        logM INFO $ "Auto restarting input " ++ show i

-- Process configured inputs.
processInputs :: SessionId
    -> (Priority -> String -> IO ())
    -> (Event UdpContent -> STM ())
    -> STM (Map.Map Channel UdpIn)
    -> TVar InputStatus
    -> IO ()
processInputs sesId logM consume getInputs inputStatus = do
    logM INFO "starting process"
    loop mempty `finally` terminate
  where
    terminate = mask $ \_restore -> do
        atomically (readTVar inputStatus) >>= mapM_ (cancel . inProcess)
        atomically (writeTVar inputStatus mempty)

    onStart ch i = do
        logM INFO $ "Starting input, ch: " ++ show ch ++ ", " ++ show i
        mask $ \restore -> do
            p <- restore $ singleInput sesId logM consume ch i
            link $ inProcess p
            atomically $ modifyTVar' inputStatus $ Map.insert ch p

    onStop ch i = do
        logM INFO $ "Stopping input, ch: " ++ show ch ++ ", " ++ show i
        mask $ \_restore -> do
            Just p <- atomically $ do
                orig <- readTVar inputStatus
                modifyTVar' inputStatus $ Map.delete ch
                return $ Map.lookup ch orig
            cancel $ inProcess p

    onRestart ch (i,j) = do
        onStop ch i
        onStart ch j

    loop current = do
        newCfg <- atomically $ do
            newCfg <- getInputs
            when (newCfg == current) retrySTM
            return newCfg

        let removed = current `Map.difference` newCfg
            added = newCfg `Map.difference` current
            checkF k v = do
                oldParam <- Map.lookup k current
                newParam <- Map.lookup k newCfg
                guard $ newParam /= oldParam
                return (v, newParam)
            changed = Map.mapMaybeWithKey checkF current

        _ <- Map.traverseWithKey onStop removed
        _ <- Map.traverseWithKey onStart added
        _ <- Map.traverseWithKey onRestart changed

        loop newCfg

runRecorder ::
    (Priority -> String -> IO ())       -- log message
    -> STM (Maybe (FilePath, Rotate))   -- get configuration
    -> STM (Event UdpContent)           -- get value to write
    -> IO ()
runRecorder logM' getConfig fetchEvent = do
    logM' INFO "startup"
    cfg <- atomically getConfig
    go cfg
  where
    logM = liftIO . logM' INFO . T.unpack

    -- process input queue while monitoring config change
    src cfg = fix $ \loop -> do
        event <- liftIO $ atomically
            (fmap Left cfgChange `STM.orElse` fmap Right fetchEvent)
        case event of
            Left cfg' -> return cfg'
            Right val -> do
                yield val
                loop
      where
        cfgChange = do
            newConfig <- getConfig
            when (newConfig == cfg) retrySTM
            return newConfig

    -- run until config change, then restart with new config
    go cfg = do
        let dst = case cfg of
                Nothing -> forever (void await)
                Just (base, rotate) ->
                    let dirArchive :: DirectoryArchive
                        dirArchive = DirectoryArchive TextEncoding base
                    in jsonRecorder (mkDirectoryRecorder (dirArchive, rotate)) logM
        PS.runSafeT (runEffect (src cfg >-> dst)) >>= go

runCmd :: CmdOptions -> Prog -> Args -> Version -> GhcBase -> WxcLib -> IO ()
runCmd opt pName pArgs version _ghc _wxcLib = do
    startTimeMono <- getMonoTimeNs
    startTimeUtc <- getUtcTime

    logM <- case optConfig opt of
        ConfigArguments _cfg True -> return noLogger -- no logging on bootstrap
        _ -> setupLogging pName "capture" (optVerbose opt) (optSyslog opt) Nothing

    logM "main" INFO $ "startup " ++ show pName ++ ", " ++ version ++ ", " ++ show pArgs
    logM "main" INFO $ show opt

    sesId <- Data.UUID.toText <$> nextRandom
    logM "main" INFO $ "session: " ++ show sesId

    config <- newTVarIO emptyConfig
    inputStatus <- newTVarIO Map.empty

    q <- newTBQueueIO 10000

    proceed <- case optConfig opt of
        ConfigArguments cfg True -> do
            BSL.putStr $ encodeJSONPrettyL cfg
            return False
        ConfigArguments cfg False -> do
            atomically $ writeTVar config cfg
            return True
        ConfigFile path _httpFlag sigHupFlag -> do
            let loadConfig = do
                    logM "main" INFO $ "Loading configuration from " ++ show path
                    result <- tryIO $ do
                        s <- BSL.readFile path
                        case eitherDecode s of
                            Right val -> return val
                            Left e -> fail e
                    case result of
                        Left e -> do
                            let msg = "Error loading file: " ++ show e
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
            -- periodic uptime logging
            [ periodic 600 $ do
                uptime <- uptimeDaysStr startTimeMono <$> getMonoTimeNs
                logM "main" INFO $ "uptime: " ++ uptime ++ ", " ++ version

            -- http server
            , httpServer (logM "main") startTimeMono startTimeUtc sesId config (readTVar inputStatus) (optConfig opt)
                `whenSpecified` (optHttp opt)

            -- recorder
            , do
                let getConfig = confOutputFile <$> readTVar config
                runRecorder (logM "recorder") getConfig (readTBQueue q)

            -- input processing
            , processInputs sesId (logM "processInputs")
                (writeTBQueue q)
                (confInputs <$> readTVar config)
                inputStatus
            ]

        logM "main" NOTICE $ "process terminated, index: " ++ show ix

-- | toplevel command
cmdCapture :: ParserInfo Command
cmdCapture = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Event recorder")

