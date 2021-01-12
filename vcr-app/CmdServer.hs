{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdServer where

-- standard imports
import           Control.Monad.Trans.Except
import           Control.Exception (try, IOException)
import           GHC.Generics (Generic)
import           Options.Applicative
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSBB
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Types
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AesonP
import           Pipes
import qualified Pipes.Safe as PS
import qualified Pipes.Prelude as PP

-- local imports
import           Common
import           Vcr
import           Time
import           Udp
import           Streaming.Disk
import           Streaming.Http

data Store
    = StoreFile FilePath
    | StoreDir FilePath
    | StoreHttp String
    deriving (Generic, Eq, Show)

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { optVerbose    :: Maybe Priority
    , optSyslog     :: Maybe Priority
    , optStore      :: Store
    , optHttp       :: (String, Warp.Port)
    } deriving (Generic, Eq, Show)

options :: Parser CmdOptions
options = CmdOptions
    <$> optional (option auto
        ( short 'v' <> long "verbose" <> metavar "LEVEL"
       <> help ("Set console verbosity level, one of: " ++ show levels)))
    <*> optional (option auto
        ( long "syslog" <> metavar "LEVEL"
       <> help ("Set syslog verbosity level, one of: " ++ show levels)))
    <*> (storeFile <|> storeDir <|> storeHttp)
    <*> httpOptions
  where
    levels = [minBound..maxBound] :: [Priority]
    httpOptions = (,)
        <$> strOption (long "http" <> metavar "IP")
        <*> option auto (long "httpPort" <> metavar "PORT")

    storeFile = StoreFile
        <$> strOption (long "file" <> metavar "FILE" <> help "file backend")
    storeDir = StoreDir
        <$> strOption (long "dir" <> metavar "FILE" <> help "directory backend (base filename)")
    storeHttp = StoreHttp
        <$> strOption (long "url" <> metavar "URL" <> help "url backend")

-- | Encode (pretty) to JSON.
encodePretty :: (Data.Aeson.ToJSON a) => a -> BSL.ByteString
encodePretty = AesonP.encodePretty'
    AesonP.defConfig {AesonP.confCompare = compare}

httpServer ::
    (Priority -> String -> IO ())
    -> MonoTimeNs
    -> UtcTime
    -> Store
    -> (String, Warp.Port)
    -> IO ()
httpServer logM startTimeMono startTimeUtc store (ip, port) = do
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

        withLog ::
            ([Text] -> StdMethod -> IO ResponseReceived)
            -> IO ResponseReceived
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

        getArg :: BS8.ByteString -> Maybe (Maybe BS8.ByteString)
        getArg label = lookup label (queryString request)

        -- process argument's value
        withArgValue ::
            BS8.ByteString   -- label
            -> m a -- action if argument not present
            -> m a -- action if argument is present, but without value
            -> (BS8.ByteString -> m a) -- action on argument's value
            -> m a
        withArgValue a f1 f2 f3 = case getArg a of
            Nothing -> f1
            Just Nothing -> f2
            Just (Just val) -> f3 val

        -- | Get index argument.
        getIndex :: (Monad m, FromJSON ix) => BS.ByteString -> ExceptT (Status, String) m ix
        getIndex ix = withArgValue ix
                (throwE (status400, show ix <> " query string not present"))
                (throwE (status400, show ix <> " argument not present"))
                (except . onError status400 . eitherDecodeStrict')

        -- | Return 'pipe' that maybe limits number of items.
        getLimit :: (Monad m, Functor f) => ExceptT String m (Pipe (ix,a) (ix,a) f ())
        getLimit = withArgValue "limit"
            (return cat)        -- no limit, identity pipe
            (throwE "limit argument not present")
            ((either throwE (return . PP.take)) <$> eitherDecodeStrict')

        respondJson :: ToJSON a => Either (Status, String) a -> IO ResponseReceived
        respondJson = \case
            Left (status, err) -> do
                logM DEBUG err
                respond $ responseLBS status
                    [("Content-Type", "text/plain")] (BSL8.pack $ err ++ "\n")
            Right result -> respond $ responseLBS status200
                [("Content-Type", "application/json")]
                (jsonEncFormat result)

        onError :: Status -> Either e a -> Either (Status,e) a
        onError status = either (\e -> Left (status,e)) Right

        player :: Player (PS.SafeT IO) Index UdpEvent
        player = case store of
            StoreFile path -> reindex $
                mkFilePlayer $ FileArchive TextEncoding path
            StoreDir base -> reindex $
                mkDirectoryPlayer $ DirectoryArchive TextEncoding base
            StoreHttp url ->
                mkHttpPlayer $ HttpArchive url

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

        go ["limits"] GET = do
            result <- PS.runSafeT $ limits player
            respond $ responseLBS status200
                [("Content-Type", "application/json")]
                (encode result)

        go ["middle"] GET = runExceptT act >>= respondJson where
            act = do
                ix1 <- getIndex "ix1"
                ix2 <- getIndex "ix2"
                lift $ PS.runSafeT $ middle player ix1 ix2

        go ["peek"] GET = runExceptT act >>= respondJson where
            act = do
                ix <- getIndex "ix"
                event <- lift $ PS.runSafeT $ peekItem player ix
                return (event :: UdpEvent)

        go ["next"] GET = runExceptT act >>= respondJson where
            act = do
                t <- withArgValue "t"
                    (throwE (status400, "'t' query string not present"))
                    (throwE (status400, "'t' argument not present"))
                    (except . onError status400 . eitherDecodeStrict')

                result <- lift $ PS.runSafeT $ findEventByTimeUtc player t
                case result of
                    Nothing -> return Nothing
                    Just (ix, _ :: UdpEvent) -> return (Just ix)

        -- stream events from the recording files
        --  - optionally starting from some index
        --  - optionally limit number of events
        --  - optionally generate (index, event) pairs, so that a client can resume request
        --  - optionally filter events, based on channel name (to reduce bandwidth)
        --      if items are filtered, respect timeout argument and send a timestamp
        --      from time to time
        --  - optionally run backwards
        go ["events"] GET = do
            eArgs <- runExceptT $ do
                direction <- withArgValue "backward"
                    (pure Forward)
                    (pure Backward)
                    (\_ -> throwE "unexpected argument value: ")
                ix <- withArgValue "ix"
                        (do
                            (ix1, ix2) <- lift $ PS.runSafeT $ limits player
                            return $ case direction of
                                Forward -> ix1
                                Backward -> ix2
                        )
                        (throwE "'ix' argument not present")
                        (either throwE pure . eitherDecodeStrict')
                limit <- getLimit
                includeIndex <- getIncludeIndex
                timeout <- withArgValue "timeout"
                    (pure Nothing)
                    (throwE "timeout argument not present")
                    (either throwE pure . eitherDecodeStrict')
                flt <- withArgValue "ch"
                    (pure Nothing)
                    (throwE "ch argument not present")
                    (either throwE (pure . Just) . eitherDecodeStrict')
                let producer :: Producer (Index, UdpEvent) (PS.SafeT IO) ()
                    producer = mkPlayer player direction ix $ case flt of
                        Nothing -> Nothing
                        Just val -> Just (val, timeout)
                return (producer >-> limit, includeIndex)
            case eArgs of
                Left err -> do
                    logM NOTICE err
                    respond $ responseLBS status400
                        [("Content-Type", "text/plain")] (BSL8.pack $ err ++ "\n")
                Right (producer, includeIndex) -> respond $ responseStream status200 [] $
                    \write flush -> do
                        let effect = for producer $ \(ix, event) -> liftIO $ do
                                let line = case includeIndex of
                                        False -> encodeJSON event
                                        True  -> encodeJSON (ix, event)
                                write $ BSBB.byteString $ line <> "\n"
                                flush
                        try (PS.runSafeT $ runEffect effect) >>= \case
                            Left (e :: IOException) -> logM DEBUG $ show e
                            Right _ -> return ()
          where
            getIncludeIndex :: Monad m => ExceptT String m Bool
            getIncludeIndex = withArgValue "includeIndex"
                (return False)
                (return True)
                (\_ -> throwE $ "unexpected argument value: ")

        go _ _ = notFound

        notFound = respond $ responseLBS
            status404
            [("Content-Type", "text/plain")]
            "404 - Not Found\n"

runCmd :: CmdOptions -> Prog -> Args -> Version -> GhcBase -> WxcLib -> IO ()
runCmd opt pName pArgs version _ghc _wxcLib = do
    startTimeMono <- getMonoTimeNs
    startTimeUtc <- getUtcTime

    logM <- setupLogging pName "server" (optVerbose opt) (optSyslog opt) Nothing

    logM "main" INFO $ "startup " ++ show pName ++ ", " ++ version ++ ", " ++ show pArgs
    logM "main" INFO $ show opt

    ix <- runAll
        -- periodic uptime logging
        [ periodic 600 $ do
            uptime <- uptimeDaysStr startTimeMono <$> getMonoTimeNs
            logM "main" INFO $ "uptime: " ++ uptime ++ ", " ++ version

        -- http server
        , httpServer (logM "main") startTimeMono startTimeUtc (optStore opt) (optHttp opt)
        ]

    logM "main" NOTICE $ "process terminated, index: " ++ show ix

-- | toplevel command
cmdServer :: ParserInfo Command
cmdServer = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Event recorder")

