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
import           Control.Monad hiding (forever)
import           Control.Monad.IO.Class (liftIO)
import           Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(DEBUG, INFO, NOTICE))
import           Control.Concurrent (threadDelay)
import           Data.Monoid
import           Database.HDBC as DB
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Database.HDBC.PostgreSQL (connectPostgreSQL)
import qualified Data.Text
import qualified Data.ByteString as BS
import           Text.Read (readMaybe)
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Types

-- local imports
import           Common (logM)
import qualified Common as C
import qualified Event
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
    , ", sesId VARCHAR(255)"
    , ", monoTime BIGINT"
    , ", value BLOB"
    , ", constraint events_pk primary key (ch, sesId, monoTime)"
    , ")"
    ]) []

-- | Deposit event to a database.
deposit :: (IConnection conn) => conn -> Event.Event -> IO Integer
deposit conn event = DB.run conn
    "INSERT OR REPLACE INTO events VALUES (?,?,?,?,?,?)"
    [ toSql $ show $ Event.eChannel event
    , toSql $ show $ Event.eSourceId event
    , toSql $ Event.unUtc $ Event.eUtcTime event
    , toSql $ show $ Event.eSessionId event
    , toSql $ Event.monoTimeToNanoSecs $ Event.eMonoTime event
    , toSql $ Event.eValue event
    ]

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

    go ["events"] GET = undefined

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
            Left (_ :: Control.Exception.SomeException) -> respond $ responseLBS
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
            -> Consumer (Either (String, BS.ByteString) Event.Event)
        writer (Connector name connect) = mkConsumer action where
            acquire = do
                liftIO $ logM DEBUG $ "open database " ++ show name
                ConnWrapper <$> (liftIO connect)
            release conn = liftIO $ do
                rv <- Control.Exception.try $ commit conn
                case rv of
                    Left (_e :: Control.Exception.SomeException) ->
                        logM NOTICE "unable to commit messages"
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
                            show (Event.eChannel evt, Event.eUtcTime evt)
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

