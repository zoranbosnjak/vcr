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
import           Control.Exception (SomeException, bracket, try, onException)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
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
    = StoreSQLite3 FilePath     -- sqlite3 file
    | StorePostgreSQL String    -- postgresql database
    {-
    -- | StoreRemote Http          -- remote server (cascade)
    -- | StoreMemory Integer       -- in memory up to N events (for test)
    -}
    deriving (Eq, Show)

data Connector = forall c. (DB.IConnection c) => Connector String (IO c)

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

connector :: Store -> Connector
connector = \case
    StoreSQLite3 path ->
        Connector ("sqlite " ++ path) (connectSqlite3 path)
    StorePostgreSQL addr ->
        Connector ("postgreSQL " ++ addr) (connectPostgreSQL addr)

-- | Run command.
runCmd :: CmdServe.Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "serve, opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    let db = connector $ optStore opts

    case optPrepare opts of
        -- (re)create tables
        True -> do
            logM INFO "preparing database..."
            _ <- withDatabase db createTables
            return ()
        -- normal operation (assume tables are prepared)
        False -> do
            runAll
                [ Process "cleanup" $ case optCleanup opts of
                    Nothing -> doNothing
                    Just (Period period, age) -> forever $ do
                        dbCleanup db age
                        threadDelaySec period
                , Process "http" $ case optServe opts of
                    ServerHttp _ip port -> Warp.run port $ app db
                ]

-- | Remove old events from database
dbCleanup :: Connector -> Age -> IO ()
dbCleanup db (Age age) = withDatabase db $ \conn -> do
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
    n <- DB.run conn cmd []
    logM INFO $
        show n ++ " item(s) removed"
        ++ ", utcTime < " ++ show t1
        ++ ", " ++ show age ++ " day(s)"

-- | Connect to a database, run some action, auto commit and disconnect.
withDatabase :: Connector -> (DB.ConnWrapper -> IO a) -> IO a
withDatabase (Connector _name connect) f =
    Control.Exception.bracket acquire disconnect action
  where
    acquire = DB.ConnWrapper <$> connect
    action conn = DB.withTransaction conn f

-- | Create database tables.
createTables :: (DB.IConnection conn) => conn -> IO Integer
createTables conn = DB.run conn (unwords
    [ "CREATE TABLE IF NOT EXISTS events"
    , "( ch VARCHAR(255)"
    , ", srcId VARCHAR(255)"
    , ", utcTime DATETIME"
    , ", monoTime BIGINT"
    , ", sesId VARCHAR(255)"
    , ", trkId VARCHAR(255)"
    , ", seqNum INT"
    , ", value BLOB"
    , ", constraint events_pk primary key (ch, sesId, monoTime)"
    , ")"
    ]) []

-- | Deposit event to a database.
deposit :: (DB.IConnection conn) => conn -> E.Event -> IO Integer
deposit conn event = DB.run conn
    "INSERT OR REPLACE INTO events VALUES (?,?,?,?,?,?,?,?)"
    [ toSql $ E.eChannel event
    , toSql $ E.eSourceId event
    , toSql $ E.eUtcTime event
    , toSql $ E.eMonoTime event
    , toSql $ E.eSessionId event
    , toSql $ E.eTrackId event
    , toSql $ E.eSequence event
    , toSql $ E.eValue event
    ]

-- | Convert SQL row to event.
toEvent :: [DB.SqlValue] -> ConvertResult E.Event
toEvent [ch, src, utc, mono, ses, trk, sn, val] = E.Event
    <$> safeFromSql ch
    <*> safeFromSql src
    <*> safeFromSql utc
    <*> safeFromSql mono
    <*> safeFromSql ses
    <*> safeFromSql trk
    <*> safeFromSql sn
    <*> safeFromSql val
toEvent val = convError "unexpected number of columns" val

-- | Format UTC time for SQL (YYYY-MM-DD HH:MM:SS.SSS)
fmtTime :: UTCTime -> String
fmtTime t = "\"" ++ (unwords $ take 2 $ words $ show t) ++ "\""

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
        | a == name2 = Just $ show $ T.unpack $ TE.decodeUtf8 b
        | otherwise = Nothing
    check _ = Nothing

app :: Connector -> Application
app db request respond = withLog go where

    -- response check
    go ["ping"] GET = respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "pong"

   -- to simulate processing delay, visit /delay/{seconds}
   -- this url is just to confirm, that server is responding to
   -- other requests, while servicing a long request
    go ["delay", s] GET = case readMaybe (T.unpack s) of
        Nothing -> respond $ responseLBS
            status400
            [("Content-Type", "text/plain")]
            "unable to decode delay value"
        Just (val::Double) -> do
            threadDelaySec val
            respond $ responseLBS
                status200
                [("Content-Type", "text/plain")]
                "ok"

    -- find list of distinct channels (within optional time interval)
    go ["info", "channels"] GET = runSelect sel stream where
        sel = return $ "SELECT DISTINCT ch from events"
            ++ optionalTimeInterval ++ " ORDER BY ch;"
        stream select write = mkEffect $ do
            rows <- liftIO $ DB.fetchAllRows' select
            forM_ (concat rows) $ \row -> liftIO $ do
                write $ fromByteString $ BS.concat [DB.fromSql row, "\n"]

    -- find list of distinct recorders (within optional time interval)
    go ["info", "recorders"] GET = runSelect sel stream where
        sel = return $ "SELECT DISTINCT srcId from events"
            ++ optionalTimeInterval ++ " ORDER BY srcId;"
        stream select write = mkEffect $ do
            rows <- liftIO $ DB.fetchAllRows' select
            forM_ (concat rows) $ \row -> liftIO $ do
                write $ fromByteString $ BS.concat [DB.fromSql row, "\n"]

    -- find UTC time of the oldest/youngest event (from opt. ch list)
    go ["info", "oldest"] GET = timeSpan "ASC"
    go ["info", "youngest"] GET = timeSpan "DESC"

    go ["events"] HEAD = undefined

    go ["events"] GET = runSelect statement stream where
        statement = do
            t1 <- maybe (fail "t1 required") return (getQueryTime request "t1")
            t2 <- maybe (fail "t2 required") return (getQueryTime request "t2")
            let
                chList = getQueryArray request ("ch", "channel")
                recList = getQueryArray request ("srcId", "recorder")
                limit = (getQueryValue request "limit") :: Maybe Integer

            return $
                "SELECT * from events WHERE"
                ++ " utcTime >= " ++ fmtTime t1
                ++ " AND"
                ++ " utcTime < "  ++ fmtTime t2
                ++ maybe "" (\x -> " AND " ++ x) chList
                ++ maybe "" (\x -> " AND " ++ x) recList
                ++ " ORDER BY ch, sesId, monoTime ASC"
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

    -- TODO: handle different content types
    -- TODO: maxSize param
    -- TODO: handle database exceptions
    go ["events"] PUT = withDatabase db $ \conn -> do
        rv <- try $ runStream $
            reader
            >-> Encodings.fromByteString maxSize fmt
            >-> writer conn
        case rv of
            Left (e :: SomeException) -> do
                liftIO $ logM NOTICE $ "error: " ++ show e
                respond $ responseLBS
                    status503
                    [("Content-Type", "text/plain")]
                    "error"
            Right _ -> respond $ responseLBS
                status200
                [("Content-Type", "text/plain")]
                "ok"
      where
        fmt = Encodings.EncJSON
        maxSize = 100*1024

        reader :: Producer BS.ByteString
        reader = mkProducer f where
            f produce = do
                chunk <- liftIO $ requestBody request
                case BS.null chunk of
                    True -> return ()
                    False -> do
                        _ <- produce chunk
                        f produce

        writer :: DB.ConnWrapper
            -> Consumer (Either (String, BS.ByteString) E.Event)
        writer conn = mkConsumer $ \consume -> forever $ do
            eVal <- consume Clear
            case eVal of
                Left (msg, _bs) -> fail msg
                Right evt -> liftIO $ do
                    _ <- deposit conn evt
                    logM DEBUG $
                        show (E.eChannel evt, E.eUtcTime evt)
                        ++ " saved."

    go ["events"] DELETE = undefined

    -- not found
    go _ _ = respond $ responseLBS
        status404
        [("Content-Type", "text/plain")]
        "404 - Not Found"

    withLog act = case parseMethod (requestMethod request) of
        Left _ -> respond $ responseLBS
            status400
            [("Content-Type", "text/plain")]
            "400 - unknown request method"
        Right mtd -> do
            logM DEBUG $ show request
            act (pathInfo request) mtd

    runSelect getStatement stream = withDatabase db $ \conn -> do
        eSelect <- Control.Exception.try $ do
            statement <- getStatement
            logM DEBUG statement
            select <- DB.prepare conn statement
            _ <- DB.execute select []
            return select

        case eSelect of
            Left (e :: SomeException) -> do
                logM NOTICE $ "database error: " ++ show e
                respond $ responseLBS
                    status503
                    [("Content-Type", "text/plain")]
                    "database error"
            Right select -> do
                let action = respond $ responseStream
                        status200
                        [("Content-Type", "text/plain")]
                        $ \write flush -> do
                            runStream $ stream select write
                            flush
                    cleanup = do
                        logM NOTICE "streaming error"
                        respond $ responseLBS
                            status503
                            [("Content-Type", "text/plain")]
                            "streaming error"
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
        sel = return $
            "SELECT utcTime from events"
            ++ chList
            ++ " ORDER BY utcTime " ++ order
            ++ " LIMIT 1;"
        stream select write = mkEffect $ do
            rows <- liftIO $ DB.fetchAllRows' select
            case concat rows of
                [] -> fail "no records found"
                [t] ->
                    let t' :: UTCTime
                        t' = DB.fromSql t :: UTCTime
                    in liftIO $ write $ fromByteString $ BS.concat
                        [ BSL.toStrict $ Encodings.jsonEncode t', "\n" ]
                _ -> fail "multiple records found"

