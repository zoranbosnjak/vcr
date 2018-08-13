------------------
-- |
-- Module: CmdRecord
--
-- 'record' command
--
-- TODO:
--  - IPv6 support on unicast/multicast input "[ipv6]", "ip" syntax
--  - IPv6 support for server output over http
--  - reduce http send rate after connection reestablished

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CmdRecord (cmdRecord) where

-- standard imports
import           Data.Monoid ((<>))
import           Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(..))
import           Data.Text (pack)
import           Network.BSD (getHostName)
import           Data.Maybe (fromMaybe)
import qualified Data.UUID
import           Data.UUID.V4 (nextRandom)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent.STM hiding (check)
import qualified Data.Map as Map
import           GHC.Generics (Generic)
import qualified Data.Aeson
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, (.=), object)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString as BS
import qualified Control.Concurrent.Async as Async
import           System.Posix.Signals
                    (installHandler, sigHUP, Handler(Catch))
import           Control.Exception (SomeException, try, mask, finally)
import           Control.Monad (when, guard)
import qualified Data.Set as Set
import           Numeric (showFFloat)
import           Data.Text (unpack)

-- local imports
import qualified Common as C
import qualified Event
import           Common (logM)
import           Streams
import           Process
import qualified Udp
import qualified Encodings as Enc
import qualified File
import qualified Buffer
import qualified Server

cmdRecord :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdRecord = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event recorder")

-- | Top level commands.
data TopCommand
    = TopFile FilePath
    | TopCmd CmdLine
    | TopDump FilePath
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
    , optOutFile    :: Maybe OutputFile
    , optOutServer  :: Maybe OutputServer
    , optBootstrap  :: Bool
    } deriving (Eq, Show)

-- | File output.
data OutputFile = OutputFile
    { outFileStore  :: File.FileStore
    , outFileEnc    :: Enc.EncodeFormat
    , outFileRotate :: (Maybe File.Rotate)
    , outFileBuffer :: Maybe Int
    } deriving (Generic, Eq, Show)

instance ToJSON OutputFile
instance FromJSON OutputFile

-- | Eventual application configuration.
data AppConfig = AppConfig
    { appStdin :: Maybe Event.Channel
    , appInputs :: Inputs
    , appFileOutput :: Maybe OutputFile
    , appServerOutput :: Maybe OutputServer
    } deriving (Generic, Eq, Show)

instance ToJSON AppConfig
instance FromJSON AppConfig

newtype Inputs = Inputs (Map.Map Event.Channel Udp.UdpIn)
    deriving (Generic, Eq, Show)

instance Monoid Inputs where
    mempty = Inputs mempty
    mappend (Inputs i1) (Inputs i2) = Inputs (mappend i1 i2)

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

    parseConfigFile :: Opt.Parser FilePath
    parseConfigFile = Opt.argument Opt.str (Opt.metavar "FILE")

    parseConfigArgs :: Opt.Parser CmdLine
    parseConfigArgs = CmdLine
        <$> Opt.optional stdinOptions
        <*> (Map.fromList <$> Opt.many udpInputOptions)
        <*> Opt.optional storeFileOptions
        <*> Opt.optional storeServerOptions
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

storeFileOptions :: Opt.Parser OutputFile
storeFileOptions = C.subparserCmd "outfile ..." $ Opt.command "outfile" $
    Opt.info
        (opts <**> Opt.helper)
        (Opt.progDesc "Store data to a (rotating) file")
  where
    opts = OutputFile
        <$> File.fileStoreOptions
        <*> Enc.encodeFormatOptions
        <*> Opt.optional File.rotateOptions
        <*> Opt.optional (Opt.option Opt.auto
            ( Opt.long "buffer"
           <> Opt.help "Internal buffer size"
            ))

-- | Server output.
data OutputServer = OutputServer
    { outServerConnection   :: Server.ServerConnection
    , outServerOnOverflow   :: OnOverflow
    , outServerLimitDrop    :: Buffer.Threshold
    } deriving (Generic, Eq, Show)

instance ToJSON OutputServer
instance FromJSON OutputServer

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

storeServerOptions :: Opt.Parser OutputServer
storeServerOptions = C.subparserCmd "server ..." $ Opt.command "server" $
    Opt.info
        (opts <**> Opt.helper)
        (Opt.progDesc "Store data to server")
  where
    opts = OutputServer
        <$> Server.serverConnectionOptions
        <*> (onOverflowDrop <|> onOverflowFile)
        <*> Buffer.thresholdOptions "drop"

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

-- | Convert raw data to Event.
toEvents :: Event.SessionId -> Event.Channel -> Event.SourceId
    -> Pipe BS.ByteString Event.Event ()
toEvents sessionId ch recId = mkPipe $ \consume produce -> do
    -- Each time when this pipe starts, a new trackId is used
    trackId <- liftIO $ Event.trackId . Data.UUID.toString <$> nextRandom
    liftIO $ logM INFO $ show ch ++ " -> " ++ show trackId
    let loop seqNum = do
            msg <- consume Clear
            (t1,t2) <- liftIO $ Event.now
            produce $ Event.Event
                { Event.eChannel = ch
                , Event.eSourceId = recId
                , Event.eUtcTime = t1
                , Event.eMonoTime = t2
                , Event.eSessionId = sessionId
                , Event.eTrackId = trackId
                , Event.eSequence = seqNum
                , Event.eValue = msg
                }
            loop $ Event.nextSequenceNum seqNum
    loop minBound

-- | Process inputs.
processInputs :: STM Inputs -> ((Event.Channel, Udp.UdpIn) -> IO ()) -> IO ()
processInputs getInputs act = do
    active <- newTVarIO mempty
    loop active mempty `finally` terminate active
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
        a <- mask $ \restore -> do
            a <- Async.async $ restore $ autoRestart ch param
            atomically $ modifyTVar' active $ Set.insert a
            return a
        return (a, param)

    onStop active ch (a, param) = do
        logM INFO $ "Stop input: " ++ show ch ++ ", " ++ show param
        mask $ \_restore -> do
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
    , appFileOutput = Nothing
    , appServerOutput = Nothing
    }

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = case optArgs opts of

  TopDump f -> do
    s <- BSL.readFile f
    case Enc.jsonEitherDecode s of
        Right val -> do
            BSL.putStr $ Enc.jsonEncodePretty (val :: AppConfig)
        Left e -> fail e

  _ -> do
    (startTimeUtc, startTimeMono) <- Event.now
    logM INFO $ "startup " ++ show startTimeUtc
    logM INFO $
        "command 'record', opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts
    recId <- do
        hostname <- Event.sourceId . pack <$> getHostName
        return $ fromMaybe hostname $ optIdent opts
    logM INFO $ show recId

    sessionId <- liftIO $ Event.sessionId . Data.UUID.toString <$> nextRandom
    logM INFO $ show sessionId

    -- common channel for all inputs
    hub <- newBroadcastTChanIO
    let rx = mkConsumer $ \consume -> forever $ do
            val <- consume Clear
            liftIO $ atomically $ writeTChan hub val
        tx ch = mkProducer $ \produce -> forever $ do
            msg <- liftIO $ atomically $ readTChan ch
            produce msg

    -- Global configuration
    cfg <- newTVarIO emptyConfig

    bstr <- case optArgs opts of

        -- dynamic config from file
        TopFile f -> do
            let readConfigFile = do
                    rv <- try $ do
                        s <- BSL.readFile f
                        case Enc.jsonEitherDecode s of
                            Right val -> do
                                logM INFO $ "Reading config success: "
                                    ++ show val
                                atomically $ writeTVar cfg val
                            Left e -> fail e
                    case rv of
                        Left (e :: SomeException) -> do
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
                    , appFileOutput = optOutFile args
                    , appServerOutput = optOutServer args
                    }
            atomically $ writeTVar cfg app
            return $ case optBootstrap args of
                False -> Nothing
                True -> Just app

        -- should not be here
        cmd -> fail $ "unexpected command " ++ show cmd

    case bstr of
      Just val -> BSL.putStr $ Enc.jsonEncodePretty val
      Nothing -> runAll
        [ Process "uptime" $ case optUptime opts of
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

        , Process "file output" $ do
            ch <- atomically $ dupTChan hub
            withVar "file output" (appFileOutput <$> readTVar cfg) $ \case
                Nothing -> runStream_ $ tx ch >-> drain
                Just fo -> runStream_ $ tx ch
                    >-> Enc.toByteString (outFileEnc fo)
                    >-> buffer (outFileBuffer fo)
                    >-> File.fileWriter
                        (outFileStore fo)
                        (outFileRotate fo)
                        (logM INFO)

        , Process "server output" $ do
            ch <- atomically $ dupTChan hub
            withVar "server output" (appServerOutput <$> readTVar cfg) $ \case
                Nothing -> runStream_ $ tx ch >-> drain
                Just srv -> do
                    let dropAct s
                            | BS.null s = return ()
                            | otherwise = case outServerOnOverflow srv of
                                OnOverflowDrop -> do
                                    logM NOTICE $ "dropping messages"
                                OnOverflowFile fs -> do
                                    let (Event.SessionId sid) = sessionId
                                        fn = File.filePath fs ++ "-" ++ sid
                                    logM NOTICE $ "dropping messages to a file "
                                        ++ show fn
                                    BS.appendFile fn s
                    runStream_ $ tx ch
                        >-> Enc.toByteString Enc.EncJSON
                        >-> Server.serverWriter
                            (outServerConnection srv)
                            (outServerLimitDrop srv)
                            dropAct

        , Process "stdin" $
            withVar "stdin" (appStdin <$> readTVar cfg) $ \case
                Nothing -> doNothing
                Just ch -> runStream_ $
                    File.fileReaderLines (File.FileStore "-")
                    >-> toEvents sessionId ch recId
                    >-> rx

        , Process "UDP inputs" $ do
            let getInputs = appInputs <$> readTVar cfg
            processInputs getInputs $ \(ch, udp) -> runStream_ $
                Udp.udpReader udp
                >-> Streams.map fst
                >-> toEvents sessionId ch recId
                >-> rx
        ]

