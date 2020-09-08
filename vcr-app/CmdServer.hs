
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdServer where

-- standard imports
import           Control.Monad.Trans.Except
import           Control.Exception (try, IOException)
import           GHC.Generics (Generic)
import           Options.Applicative
import           Data.String (fromString)
import qualified Data.Text.Encoding as TE
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
import           Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as TRT

-- local imports
import           Common
import           Vcr
import           Time
import           Udp
import           File

data Store
    = StoreDir FilePath
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
    <*> ((flag' () (long "dir" <> help "serve from directory")) *> baseFile)
    <*> httpOptions
  where
    levels = [minBound..maxBound] :: [Priority]
    httpOptions = (,)
        <$> strOption (long "http" <> metavar "IP")
        <*> option auto (long "httpPort" <> metavar "PORT")
    baseFile = StoreDir
        <$> strOption (long "base" <> metavar "FILE" <> help "base file")

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

        getIx :: FilePath -> ExceptT String IO Index
        getIx base = withArgValue "ix"
            (liftIO (getStartIndex base) >>= except)
            (throwE "ix argument not present")
            (except . eitherDecodeStrict)

        getLimit :: (Monad m, Functor f) => ExceptT String m (Pipe a a f ())
        getLimit = withArgValue "limit"
            (return cat)        -- no limit, identity pipe
            (throwE "limit argument not present")
            ((either throwE (return . PP.take)) <$> eitherDecodeStrict')

        decodeEvent :: BS8.ByteString -> Either String UdpEvent
        decodeEvent = eitherDecodeStrict'

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

        -- get first index from the recording
        go ["firstIndex"] GET = case store of
            StoreDir base -> getStartIndex base
                >>= respondJson . onError status503

        -- get next event index from a given index
        go ["nextIndexFromIndex"] GET = case store of
            StoreDir base -> runExceptT act >>= respondJson where
                act = do
                    ix1 <- withArgValue "ix"
                        (throwE (status400, "ix query string not present"))
                        (throwE (status400, "ix argument not present"))
                        (except . onError status400 . eitherDecodeStrict')
                    ix2 <- liftIO $ getNextIndexFromIndex base ix1
                    except $ onError status503 ix2

        -- get next event index from given UTC timestamp
        go ["nextIndexFromUtc"] GET = case store of
            StoreDir base -> runExceptT act >>= respondJson where
                checkLine s = eTimeUtc <$> decodeEvent s
                act = do
                    utc <- withArgValue "t"
                        (throwE (status400, "time query string not present"))
                        (throwE (status400, "time argument not present"))
                        (except . onError status400 . parseIsoTime . BS8.unpack)
                    ix <- liftIO $ getNextIndexFromUtc base checkLine utc
                    except $ onError status503 ix

        -- stream events from the recording files as raw bytestrings
        -- This is a faster version to retrive events,
        -- since the event decoding is not required during the process.
        --  - optionally starting from some index
        --  - optionally limit number of events
        go ["eventsRaw"] GET = case store of
            StoreDir base -> do
                eProducer <- runExceptT $ do
                    producer <- lineReader base <$> getIx base
                    limit <- getLimit
                    return (producer >-> limit >-> PP.map snd)
                case eProducer of
                    Left err -> do
                        logM NOTICE err
                        respond $ responseLBS status400
                            [("Content-Type", "text/plain")] (BSL8.pack $ err ++ "\n")
                    Right producer -> respond $ responseStream status200 [] $
                        \write flush -> do
                            let effect = for producer $ \s -> liftIO $ do
                                    write $ BSBB.byteString $ s <> "\n"
                                    flush
                            try (PS.runSafeT $ runEffect effect) >>= \case
                                Left (e :: IOException) -> logM DEBUG $ show e
                                Right _ -> return ()

        -- stream events from the recording files
        --  - optionally starting from some index
        --  - optionally limit number of events
        --  - optionally generate (index, event) pairs, so that a client can resume request
        --  - optionally filter events, based on channel name
        --      (filtered events don't include eValue, to reduce bandwidth)
        go ["events"] GET = case store of
            StoreDir base -> do
                eArgs <- runExceptT $ do
                    rawProducer <- lineReader base <$> getIx base
                    predicate <- getChPredicate
                    let producer :: Producer (Index, Either (Event ()) UdpEvent) (PS.SafeT IO) ()
                        producer = for rawProducer $ \(ix, s) -> do
                            event <- either fail pure (decodeEvent s)
                            yield (ix, case predicate event of
                                False -> Left (fmap (const ()) event)
                                True -> Right event)
                    limit <- getLimit
                    includeIndex <- getIncludeIndex
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
                                            False -> encodeCompact event
                                            True  -> encodeCompact (ix, event)
                                    write $ BSBB.byteString $ BSL.toStrict $ line <> "\n"
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

            getChPredicate :: Monad m => ExceptT String m (Event a -> Bool)
            getChPredicate = withArgValue "ch"
                (return $ const True)
                (throwE "ch argument not present")
                (\s -> case TE.decodeUtf8' s of
                    Left e -> throwE $ show e
                    Right t -> case TRT.compile defaultCompOpt (ExecOption False) t of
                        Left e -> throwE e
                        Right re -> return $ \event ->
                            let ch = eChannel event
                                result = TRT.execute re ch
                            in case result of
                                Left e -> error e
                                Right Nothing -> False
                                Right _ -> True
                )

        -- Remark:
        --  In case of complex arguments, it would be better
        --  to pass parameters in the request body, using POST method.
        --  For now, just return "Not Implemented" status.
        go ["events"] POST = respond $ responseLBS
            status501
            [("Content-Type", "text/plain")]
            "501 - Not Implemented\n"

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

