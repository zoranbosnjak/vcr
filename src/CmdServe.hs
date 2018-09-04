------------------
-- |
-- Module: CmdServe
--
-- 'serve' command
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module CmdServe (cmdServe) where

-- Standard imports.
import           Control.Exception (bracket, onException)
import           Control.Monad (forM_)
import           Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(DEBUG, INFO, NOTICE))
import           Data.Binary.Builder (fromByteString)
import           Data.Monoid
import           Data.Maybe
import           Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import           Data.Convertible
import qualified Data.Aeson
import qualified Database.HDBC as DB
import           Database.HDBC (disconnect, toSql, safeFromSql)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Database.HDBC.PostgreSQL (connectPostgreSQL)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Text.Read (readMaybe)
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Types

-- local imports
import           Common (logM, threadDelaySec)
import qualified Common as C
import qualified Event as E
import qualified Encodings
import           Streams
import           Process

cmdServe :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdServe = Opt.info ((runCmd <$> CmdServe.options) <**> Opt.helper)
    (Opt.progDesc "Server part of the recorder")

type Ip = String
type Port = Int

newtype Period = Period Double deriving (Eq, Show)
newtype Age = Age Double deriving (Eq, Show)

data Options = Options
    { optServe :: Server
    , optStore :: Store
    , optPrepare :: Bool
    , optCleanup :: Maybe (Period, Age) -- TODO: define Age per channel, default
    } deriving (Eq, Show)

options :: Opt.Parser CmdServe.Options
options = CmdServe.Options
    <$> server
    <*> store
    <*> Opt.switch
        (Opt.long "bootstrap"
            <> Opt.help "create database tables and exit")
    <*> Opt.optional ( (,)
        <$> fmap Period (Opt.option Opt.auto
            ( Opt.metavar "SEC"
           <> Opt.long "cleanupPeriod"
           <> Opt.help "trigger auto cleanup procedure every N seconds"
            ))
        <*> fmap Age (Opt.option Opt.auto
            ( Opt.metavar "DAYS"
           <> Opt.long "cleanupAge"
           <> Opt.help "auto remove records older then N days"
            ))
        )

data Server
    = ServerHttp Ip CmdServe.Port
    deriving (Eq, Show)

server :: Opt.Parser Server
server = Opt.subparser $
    Opt.command "server"
        (Opt.info (Opt.helper <*> level2) (Opt.progDesc "server definition"))
  where
    level2 = Opt.subparser $
        Opt.command "http" (Opt.info (Opt.helper <*> http) Opt.idm)
    http = ServerHttp
        <$> Opt.argument Opt.str (Opt.metavar "IP" <> Opt.help "IP address")
        <*> Opt.argument Opt.auto
            (Opt.metavar "PORT" <> Opt.help "UDP port number")

data Store
    = StoreSQLite3 FilePath     -- sqlite3 (file)
    | StorePostgreSQL String    -- postgresql (connection string)
    deriving (Eq, Show)

store :: Opt.Parser Store
store = Opt.subparser $
    Opt.command "store"
        (Opt.info (Opt.helper <*> level2) (Opt.progDesc "store definition"))
  where
    level2 = Opt.subparser
        ( Opt.command "sqlite3" (Opt.info (Opt.helper <*> sqlite3) Opt.idm)
       <> Opt.command "postgres" (Opt.info (Opt.helper <*> postgres) Opt.idm)
        )
    sqlite3 = StoreSQLite3
        <$> Opt.argument Opt.str (Opt.metavar "PATH" <> Opt.help "File path.")
    postgres = StorePostgreSQL
        <$> Opt.argument Opt.str
            (Opt.metavar "STRING" <> Opt.help "Connection string.")

connect :: Store -> IO DB.ConnWrapper
connect = \case
    StoreSQLite3 path -> DB.ConnWrapper <$> connectSqlite3 path
    StorePostgreSQL addr -> DB.ConnWrapper <$> connectPostgreSQL addr

-- | Run command.
runCmd :: CmdServe.Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "serve, opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    let dbType = optStore opts
    Control.Exception.bracket (connect dbType) disconnect $ \conn ->
      case optPrepare opts of
        -- create tables and exit
        True -> do
            logM INFO "preparing database..."
            DB.withTransaction conn $ \_ -> do
                _ <- DB.run conn (createTables dbType) []
                return ()
        -- normal operation (assume tables are prepared)
        False -> do
            runAll
                [ Process "cleanup" $ case optCleanup opts of
                    Nothing -> doNothing
                    Just (Period period, age) -> forever $ do
                        dbCleanup conn age
                        threadDelaySec period
                , Process "http" $ case optServe opts of
                    ServerHttp _ip port -> Warp.run port $ app dbType conn
                ]

-- | Remove old events from database
dbCleanup :: DB.IConnection conn => conn -> Age -> IO ()
dbCleanup conn (Age age) = do
    t1 <- do
        now <- getCurrentTime
        let delta = nominalDay * fromRational (toRational age)
        return $ addUTCTime (-delta) now

    let cmd = unwords
            [ "DELETE from events WHERE"
            , "utcTime <"
            , fmtTime t1
            ]

    logM DEBUG cmd
    n <- DB.withTransaction conn $ \_ -> DB.run conn cmd []
    logM INFO $
        show n ++ " item(s) removed"
        ++ ", utcTime < " ++ show t1
        ++ ", " ++ show age ++ " day(s)"

-- | Create database tables.
createTables :: Store -> String
createTables _dbType = unwords
    [ "CREATE TABLE events"
    , "( ch VARCHAR(255)"
    , ", srcId VARCHAR(255)"
    , ", utcTime timestamp with time zone"
    , ", utcTimePico BIGINT"    -- store picoseconds separately
    , ", monoTime BIGINT"
    , ", sesId VARCHAR(255)"
    , ", trkId VARCHAR(255)"
    , ", seqNum INT"
    , ", value TEXT"
    , ", constraint events_pk primary key (ch, sesId, monoTime)"
    , ")"
    ]

-- | Deposit event to a database.
deposit :: (DB.IConnection conn) => Store -> conn -> E.Event -> IO Integer
deposit dbType conn event = DB.run conn
    insertStatement
    [ toSql $ E.eChannel event
    , toSql $ E.eSourceId event
    , toSql $ E.eUtcTime event
    , toSql $ E.getUtcPicos $ E.eUtcTime event
    , toSql $ E.eMonoTime event
    , toSql $ E.eSessionId event
    , toSql $ E.eTrackId event
    , toSql $ E.eSequence event
    , toSql $ Encodings.hexlify $ E.eValue event
    ]
  where
    insertStatement = case dbType of
        StoreSQLite3 _ ->
            "INSERT OR REPLACE INTO events VALUES (?,?,?,?,?,?,?,?,?)"
        StorePostgreSQL _ ->
            "INSERT INTO events VALUES (?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"

utcPrecise :: DB.SqlValue -> DB.SqlValue -> Either ConvertError E.UtcTime
utcPrecise utc utcP = E.replaceUtcPicos <$> safeFromSql utcP <*> safeFromSql utc

-- | Convert SQL row to event.
toEvent :: [DB.SqlValue] -> ConvertResult E.Event
toEvent [ch, src, utc, utcP, mono, ses, trk, sn, val] = E.Event
    <$> safeFromSql ch
    <*> safeFromSql src
    <*> utcPrecise utc utcP
    <*> safeFromSql mono
    <*> safeFromSql ses
    <*> safeFromSql trk
    <*> safeFromSql sn
    <*> converted
  where
    converted = do
        x <- safeFromSql val
        maybe (fail "unable to decode") return (Encodings.unhexlify x)

toEvent val = convError "unexpected number of columns" val

-- | Format UTC time for SQL (YYYY-MM-DD HH:MM:SS.SSS)
fmtTime :: UTCTime -> String
fmtTime t = "'" ++ show t ++ "'"

-- | get time from query string
getQueryTime :: Request -> BS.ByteString -> Maybe UTCTime
getQueryTime request name = do
    x <- lookup name $ queryString request
    y <- x
    Data.Aeson.decode $ BSL.fromStrict $ BS.concat["\"", y, "\""]

-- | get value from query string
getQueryValue :: (Read a) => Request -> BS.ByteString -> Maybe a
getQueryValue request name = do
    x <- lookup name $ queryString request
    y <- x
    readMaybe $ T.unpack $ TE.decodeUtf8 y

-- | get list of values from query string -> {param} in ('{val1}', '{val2}'...)
-- To be used with SQL WHERE.
getQueryArray :: Request -> (String, BS.ByteString) -> Maybe String
getQueryArray request (name1, name2) = case argList of
    [] -> Nothing
    _ -> Just $ name1 ++ " in ("
            ++ foldl1 (\a b -> a ++ "," ++ b) argList
            ++ ")"
  where
    argList = mapMaybe check $ queryString request
    check (a, Just b)
        | a == name2 = Just $ "'" ++ (T.unpack $ TE.decodeUtf8 b) ++ "'"
        | otherwise = Nothing
    check _ = Nothing

app :: DB.IConnection c => Store -> c -> Application
app db conn request respond = withLog go where

    withLog act = case parseMethod (requestMethod request) of
        Left _ -> respond $ responseLBS
            status400
            [("Content-Type", "text/plain")]
            "400 - unknown request method\n"
        Right mtd -> do
            logM DEBUG $ show request
            act (pathInfo request) mtd

    -- response check
    go ["ping"] GET = respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "pong\n"

   -- to simulate processing delay, visit /delay/{seconds}
   -- this url is just to confirm, that server is responding to
   -- other requests, while servicing a long request
    go ["delay", s] GET = case readMaybe (T.unpack s) of
        Nothing -> respond $ responseLBS
            status400
            [("Content-Type", "text/plain")]
            "unable to decode delay value\n"
        Just (val::Double) -> do
            threadDelaySec val
            respond $ responseLBS
                status200
                [("Content-Type", "text/plain")]
                "ok\n"

    -- find list of distinct channels (within optional time interval)
    go ["info", "channels"] GET = runSelect sel stream where
        sel = "SELECT DISTINCT ch from events"
            ++ optionalTimeInterval ++ " ORDER BY ch;"
        stream select write = mkEffect $ do
            rows <- liftIO $ DB.fetchAllRows' select
            forM_ (concat rows) $ \row -> liftIO $ do
                write $ fromByteString $ BS.concat [DB.fromSql row, "\n"]

    -- find list of distinct recorders (within optional time interval)
    go ["info", "recorders"] GET = runSelect sel stream where
        sel = "SELECT DISTINCT srcId from events"
            ++ optionalTimeInterval ++ " ORDER BY srcId;"
        stream select write = mkEffect $ do
            rows <- liftIO $ DB.fetchAllRows' select
            forM_ (concat rows) $ \row -> liftIO $ do
                write $ fromByteString $ BS.concat [DB.fromSql row, "\n"]

    -- find UTC time of the oldest/youngest event (from opt. ch list)
    go ["info", "oldest"] GET = timeSpan "ASC"
    go ["info", "youngest"] GET = timeSpan "DESC"

    go ["events"] GET = DB.withTransaction conn $ \_ ->
        runSelect statement stream
      where
        statement =
            let t1 = getQueryTime request "t1"
                t2 = getQueryTime request "t2"
                chList = getQueryArray request ("ch", "channel")
                recList = getQueryArray request ("srcId", "recorder")
                limit = (getQueryValue request "limit") :: Maybe Integer

            in
                "SELECT * from events WHERE NULL IS NULL"
                ++ maybe "" (\x -> " AND utcTime >= " ++ fmtTime x) t1
                ++ maybe "" (\x -> " AND utcTime < " ++ fmtTime x) t2
                ++ maybe "" (\x -> " AND " ++ x) chList
                ++ maybe "" (\x -> " AND " ++ x) recList
                ++ " ORDER BY monoTime, ch, sesId ASC"
                ++ maybe "" (\x -> " LIMIT " ++ show x) limit
                ++ ";"

        stream select write =
            reader select
            >-> Encodings.toByteString Encodings.EncJSON
            >-> sender write

        reader select = mkProducer loop where
            loop produce = do
                mRow <- liftIO $ DB.fetchRow select
                case mRow of
                    Nothing -> return ()    -- all done
                    Just row -> do
                        case toEvent row of
                            Left e -> do
                                liftIO $ logM NOTICE $
                                    "error converting data from database "
                                    ++ show e
                                fail $ show e
                            Right val -> do
                                _ <- produce val
                                return ()
                        loop produce

        sender tx = mkConsumer $ \consume -> forever $ do
            consume Clear >>= liftIO . tx . fromByteString

    go ["events"] PUT = DB.withTransaction conn $ \_ -> do
        rv <- runStream $
            reader
            >-> Encodings.fromByteString maxSize fmt
            >-> writer
        case rv of
            Just e -> do
                liftIO $ logM NOTICE $ "error: " ++ e
                respond $ responseLBS
                    status503
                    [("Content-Type", "text/plain")]
                    "error\n"
            Nothing -> respond $ responseLBS
                status200
                [("Content-Type", "text/plain")]
                "ok\n"
      where
        fmt = Encodings.EncJSON
        maxSize = 100*1024

        reader :: Producer BS.ByteString (Maybe String)
        reader = mkProducer f where
            f produce = do
                chunk <- liftIO $ requestBody request
                case BS.null chunk of
                    True -> return Nothing
                    False -> do
                        _ <- produce chunk
                        f produce

        writer ::
            Consumer (Either (String, BS.ByteString) E.Event) (Maybe String)
        writer = mkConsumer loop where
            loop consume = do
                eVal <- consume Clear
                case eVal of
                    Left (msg, _bs) -> return $ Just msg
                    Right evt -> do
                        _ <- liftIO $ deposit db conn evt
                        liftIO $ logM DEBUG $
                            show (E.eChannel evt, E.eUtcTime evt)
                            ++ " saved."
                        loop consume

    -- not found
    go _ _ = respond $ responseLBS
        status404
        [("Content-Type", "text/plain")]
        "404 - Not Found\n"

    runSelect statement stream = do
        logM DEBUG statement
        select <- DB.prepare conn statement
        _ <- DB.execute select []
        let action = respond $ responseStream
                status200
                [("Content-Type", "text/plain")]
                $ \write flush -> do
                    runStream_ $ stream select write
                    flush
            cleanup = do
                logM NOTICE "streaming error"
                respond $ responseLBS
                    status503
                    [("Content-Type", "text/plain")]
                    "streaming error\n"
        onException action cleanup

    optionalTimeInterval = case (t1, t2) of
        (Nothing, Nothing) -> ""
        (Nothing, Just b) -> " WHERE utcTime < " ++ b
        (Just a, Nothing) -> " WHERE utcTime >= " ++ a
        (Just a, Just b) -> " WHERE utcTime >= " ++ a ++ " AND utcTime < " ++ b
      where
        t1 = fmtTime <$> getQueryTime request "t1"
        t2 = fmtTime <$> getQueryTime request "t2"

    timeSpan order = runSelect sel stream where
        chList = maybe "" (" WHERE " ++) $
            getQueryArray request ("ch", "channel")
        sel =
            "SELECT utcTime, utcTimePico from events"
            ++ chList
            ++ " ORDER BY utcTime " ++ order
            ++ " LIMIT 1;"
        stream select write = mkEffect $ liftIO $ do
            rows <- DB.fetchAllRows' select
            write $ fromByteString $ BS.concat
                [ BSL.toStrict $ Encodings.jsonEncode $ val rows
                , "\n"
                ]
          where
            val rows = case concat rows of
                [] -> Nothing :: Maybe E.UtcTime    -- "no records found"
                [utc, utcP] -> either (error "unable to decode") Just
                    (utcPrecise utc utcP)
                _ -> error "multiple records found"

