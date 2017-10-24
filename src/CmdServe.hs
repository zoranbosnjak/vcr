------------------
-- |
-- Module: CmdServe
--
-- 'serve' command
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module CmdServe (cmdServe) where

-- Standard imports.
import           Control.Concurrent.Async
import qualified Control.Exception
import           Control.Exception (SomeException)
import           Control.Monad hiding (forever)
import           Control.Monad.IO.Class (liftIO)
import           Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(DEBUG, INFO, NOTICE))
import           Control.Concurrent (threadDelay)
import           Data.Binary.Builder (fromByteString)
import           Data.Monoid
import           Data.Convertible
import           Database.HDBC as DB
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Database.HDBC.PostgreSQL (connectPostgreSQL)
import           Data.Either
import qualified Data.Text
import qualified Data.ByteString as BS
import           Text.Read (readMaybe)
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Types

-- local imports
import           Common (logM)
import qualified Common as C
import qualified Event as E
--import qualified Server
--import qualified File
import qualified Encodings
import           Streams

cmdServe :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdServe = Opt.info ((runCmd <$> CmdServe.options) <**> Opt.helper)
    (Opt.progDesc "Server part of the recorder")

type Ip = String
type Port = Int
type RequestedReplicas = Int
--type ActualReplicas = Int

data Options = Options
    { optServe :: [Server]
    , optStore :: [Store]
    , optReplicas :: RequestedReplicas
    , optPrepare :: Bool
    } deriving (Eq, Show)

options :: Opt.Parser CmdServe.Options
options = CmdServe.Options
    <$> Opt.some server
    <*> Opt.some store
    <*> Opt.option Opt.auto
        (Opt.short 'n' <> Opt.long "replicas"
            <> Opt.help "requested number of replicas before acknowledge write"
            <> Opt.value 1)
    <*> Opt.switch
        (Opt.long "bootstrap"
            <> Opt.help "create database tables and exit")

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
    -- | StoreRemote Http          -- remote server (cascade)
    -- | StoreMemory Integer       -- in memory up to N events (for test)
    deriving (Eq, Show)

data Connector = forall c. (IConnection c) => Connector String (IO c)

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
connector dbType = case dbType of
    StoreSQLite3 path ->
        Connector ("sqlite " ++ path) (connectSqlite3 path)
    StorePostgreSQL addr ->
        Connector ("postgreSQL " ++ addr) (connectPostgreSQL addr)

-- | Run command.
runCmd :: CmdServe.Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "serve, opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    C.check (optReplicas opts <= length (optStore opts))
        "number of requested replicas must be <= available database connections"

    let connectors = connector <$> (optStore opts)

    case optPrepare opts of
        -- (re)create tables
        True -> do
            logM INFO "preparing databases..."
            forM_ connectors $ flip withDatabase createTables

        -- normal operation (assume tables are prepared)
        False -> do
            logM INFO "server..."
            -- start servers
            servers <- forM (optServe opts) $ \i -> liftIO $ async $ case i of
                ServerHttp _ip port -> Warp.run port $
                    app connectors (optReplicas opts)

            -- all threads shall remain running
            _ <- liftIO $ waitAnyCatchCancel servers
            C.throw "server process terminated"

-- | Connect to a database, run some action, auto commit and disconnect.
withDatabase :: Connector -> (ConnWrapper -> IO a) -> IO a
withDatabase (Connector _name connect) f =
    Control.Exception.bracket acquire disconnect action
  where
    acquire = ConnWrapper <$> connect
    action conn = withTransaction conn f

-- | Create database tables.
createTables :: (IConnection conn) => conn -> IO Integer
createTables conn = DB.run conn (unwords
    [ "CREATE TABLE IF NOT EXISTS events"
    , "( ch VARCHAR(255)"
    , ", srcId VARCHAR(255)"
    , ", utcTime DATETIME"
    , ", monoTime BIGINT"
    , ", sesId VARCHAR(255)"
    , ", seqNum INT"
    , ", value BLOB"
    , ", constraint events_pk primary key (ch, sesId, monoTime)"
    , ")"
    ]) []

-- | Deposit event to a database.
deposit :: (IConnection conn) => conn -> E.Event -> IO Integer
deposit conn event = DB.run conn
    "INSERT OR REPLACE INTO events VALUES (?,?,?,?,?,?,?)"
    [ toSql $ E.eChannel event
    , toSql $ E.eSourceId event
    , toSql $ E.eUtcTime event
    , toSql $ E.eMonoTime event
    , toSql $ E.eSessionId event
    , toSql $ E.eSequence event
    , toSql $ E.eValue event
    ]

-- | Convert SQL row to event.
toEvent :: [SqlValue] -> ConvertResult E.Event
toEvent [ch, src, utc, mono, ses, sn, val] = E.Event
    <$> safeFromSql ch
    <*> safeFromSql src
    <*> safeFromSql utc
    <*> safeFromSql mono
    <*> safeFromSql ses
    <*> safeFromSql sn
    <*> safeFromSql val
toEvent val = convError "unexpected number of columns" val

app :: [Connector] -> Int -> Application
app connectors _replicas request respond = withLog go where

    threadDelaySec = threadDelay . round . (1000000*)

    -- response check
    go ["ping"] GET = respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "pong"

   -- simulate processing delay
    go ["delay", s] GET = case readMaybe (Data.Text.unpack s) of
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

    go ["events"] HEAD = undefined

    -- TODO: add query params, see Data.Time.Format
    -- TODO: handle different content types
    go ["events"] GET = do
        -- connect to databases and run select
        candidates <- forM connectors $ \(Connector name connect) -> do
            Control.Exception.try $ do
                conn <- ConnWrapper <$> connect
                select <- prepare conn
                    "SELECT * from events ORDER BY ch, sesId, monoTime ASC;"
                _ <- execute select []
                return (name, conn, select)

        let inactive :: [SomeException]
            inactive = lefts candidates

        -- all active databases must be able to return values
        case rights candidates of
            [] -> do
                logM NOTICE $ "no database is accessible"
                    ++ ", " ++ show (length inactive) ++ " inactive databases"
                respond $ responseLBS
                    status503
                    [("Content-Type", "text/plain")]
                    "database error"
            active -> do
                let getName (name,_,_) = name
                    getCon (_,con,_) = con
                    getSelect (_,_,select) = select
                logM DEBUG $
                    "active databases: " ++ show (getName <$> active)
                    ++ ", inactive: " ++ show (length inactive)
                let src = mergeOrdStreams (toProducer . getSelect <$> active)
                    action = respond $ responseStream
                        status200
                        [("Content-Type", "text/plain")]
                        $ \write flush -> do
                            runStream $
                                src
                                >-> Encodings.toByteString fmt
                                >-> sender write
                            flush
                            logM DEBUG $
                                "closing database " ++ show (getName <$> active)
                            mapM_ disconnect (getCon <$> active)
                    cleanup = do
                        logM NOTICE "streaming error"
                        logM DEBUG $
                            "closing database " ++ show (getName <$> active)
                        mapM_ disconnect (getCon <$> active)
                        respond $ responseLBS
                            status503
                            [("Content-Type", "text/plain")]
                            "streaming error"
                Control.Exception.onException action cleanup

      where

        fmt = Encodings.EncJSON Encodings.JSONCompact

        toProducer select = mkProducer loop where
            loop produce = do
                mRow <- liftIO $ fetchRow select
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
            consume >>= liftIO . tx . fromByteString

    -- TODO: handle different content types
    -- TODO: maxSize param
    -- TODO: run in parallel, use async
    -- TODO: performance, finish when required number of replicas respond
    -- TODO: handle database exceptions
    go ["events"] PUT = do
        rv <- Control.Exception.try $ runStream $
            reader
            >-> Encodings.fromByteString maxSize fmt
            >-> forkStreams [writer c | c <- connectors]
        case rv of
            Left (_ :: SomeException) -> respond $ responseLBS
                status503
                [("Content-Type", "text/plain")]
                "database error"
            Right _ -> respond $ responseLBS
                status200
                [("Content-Type", "text/plain")]
                "ok"
      where
        fmt = Encodings.EncJSON Encodings.JSONCompact
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

        writer :: Connector
            -> Consumer (Either (String, BS.ByteString) E.Event)
        writer (Connector name connect) = mkConsumer action where
            acquire = do
                liftIO $ logM DEBUG $ "open database " ++ show name
                ConnWrapper <$> (liftIO connect)
            release conn = liftIO $ do
                rv <- Control.Exception.try $ commit conn
                case rv of
                    Left (e :: SomeException) -> logM NOTICE $
                        "unable to commit messages to "
                        ++ show name
                        ++ ": " ++ show e
                    Right _ -> return ()
                logM DEBUG $ "closing database " ++ show name
                disconnect conn
            action consume = bracket acquire release $ \conn -> forever $ do
                eVal <- consume
                case eVal of
                    Left (msg, _bs) -> liftIO $ logM NOTICE msg
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

