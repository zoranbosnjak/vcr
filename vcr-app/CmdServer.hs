
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdServer where

-- standard imports
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Exception (try, IOException)
import           GHC.Generics (Generic)
import           Options.Applicative
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
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
import           Text.Regex.TDFA
import qualified Text.Regex.TDFA.String as TRS

-- local imports
import           Common
import           Vcr
import           Time
import           Udp
import           File

data Store
    = StoreDir FilePath
    -- | StoreKafka :: ...
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

        decodeEvent :: BS8.ByteString -> Either String UdpEvent
        decodeEvent = eitherDecodeStrict

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

        -- get event index from given UTC timestamp
        -- ignore request if file output is not configured
        go ["nextIndex"] GET = case store of
            StoreDir base -> calculate base >>= dump
          where
            checkLine s = eTimeUtc <$> decodeEvent s
            calculate base = runExceptT $ do
                utc <- return (getArg "t")
                    >>= maybe (throwE "time query string not present") pure
                    >>= maybe (throwE "time argument not present") (pure . parseIsoTime . BS8.unpack)
                    >>= either throwE pure
                (liftIO $ getNextIndex base checkLine utc) >>= \case
                    Left e -> throwE e
                    Right val -> pure val
            dump = \case
                Left err -> respond $ responseLBS status400
                    [("Content-Type", "text/plain")] (BSL8.pack $ err ++ "\n")
                Right result -> respond $ responseLBS status200
                    [("Content-Type", "application/json")]
                    (jsonEncFormat result)

        -- fetch stream of events from the recording files
        --  - optionally starting from some index
        --  - optionally limit number of events
        --  - generate (event, nextIndex) pairs, so that a client can resume request
        --  - ignore request if file output is not configured
        go ["events"] method = case store of
            StoreDir base -> getParams base >>= \case
                Left err -> respond $ responseLBS status400
                    [("Content-Type", "text/plain")] (BSL8.pack $ err ++ "\n")
                Right (ix, limit, eventPredicate) -> respond $ responseStream status200 [] $
                  \write flush -> do
                    let processLine line mNextIndex = case eitherDecodeStrict line of
                            Left e -> fail e
                            Right (event :: UdpEvent) -> case eventPredicate event of
                                Left e -> fail e
                                Right predicate -> when predicate $ do
                                    write $ BSBB.byteString $ case includeIndex of
                                        False -> line <> "\n"
                                        True ->
                                            let s = decodeUtf8 . BSL.toStrict . encodeCompact <$> mNextIndex
                                            in (BSL.toStrict $ encodeCompact (event, s)) <> "\n"
                                    flush
                    try (streamFrom base processLine ix limit) >>= \case
                        Left (_e :: IOException) -> return ()
                        Right _ -> return ()
          where
            includeIndex = maybe False (const True) $ lookup "includeIndex" $ queryString request
            getParams base = runExceptT $ case method of
                GET -> do
                    ix <- return (getArg "t") >>= \case
                        Nothing -> (liftIO $ getStartIndex base) >>= either throwE pure
                        Just (Nothing) -> throwE "time argument not present"
                        Just (Just s) -> either throwE pure (eitherDecodeStrict s)
                    limit <- return (getArg "limit") >>= \case
                        Nothing -> return Nothing
                        Just (Nothing) -> throwE "limit argument not present"
                        Just (Just s) -> either throwE pure (eitherDecodeStrict s)
                    channelFilter <- return (getArg "ch") >>= \case
                        Nothing -> return (const $ Right True)
                        Just (Nothing) -> throwE "ch argument not present"
                        Just (Just s) -> case TRS.compile defaultCompOpt (ExecOption False) (BS8.unpack s) of
                            Left e -> throwE e
                            Right re -> return $ \event ->
                                let ch = T.unpack $ eChannel event
                                    result = TRS.execute re ch
                                in case result of
                                    Left e -> Left e
                                    Right Nothing -> Right False
                                    Right _ -> Right True
                    return (ix, limit, channelFilter)

                --  In the case of complex arguments, it would be more convenient
                --  to pass parameters in the request body, using POST method instead of GET.
                -- POST -> ...

                _ -> throwE "unsupported http method"

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

