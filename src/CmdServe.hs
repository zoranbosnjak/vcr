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
import           Control.Exception (SomeException, bracket, try, onException)
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

data Options = Options
    { optServe :: Server
    , optStore :: Store
    , optPrepare :: Bool
    } deriving (Eq, Show)

options :: Opt.Parser CmdServe.Options
options = CmdServe.Options
    <$> server
    <*> store
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

    let db = connector $ optStore opts

    case optPrepare opts of
        -- (re)create tables
        True -> do
            logM INFO "preparing database..."
            _ <- withDatabase db createTables
            return ()
        -- normal operation (assume tables are prepared)
        False -> do
            logM INFO "starting server"
            case optServe opts of
                ServerHttp _ip port -> Warp.run port $ app db

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
    , ", trkId VARCHAR(255)"
    , ", seqNum INT"
    , ", value BLOB"
    , ", constraint events_pk primary key (ch, sesId, monoTime)"
    , ")"
    ]) []

-- | Deposit event to a database.
deposit :: (IConnection conn) => conn -> E.Event -> IO Integer
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
toEvent :: [SqlValue] -> ConvertResult E.Event
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
    go ["delay", s] GET = case readMaybe (Data.Text.unpack s) of
        Nothing -> respond $ responseLBS
            status400
            [("Content-Type", "text/plain")]
            "unable to decode delay value"
        Just (val::Double) -> do
            let threadDelaySec = threadDelay . round . (1000000*)
            threadDelaySec val
            respond $ responseLBS
                status200
                [("Content-Type", "text/plain")]
                "ok"

    go ["events"] HEAD = undefined

    -- TODO: add query params, see Data.Time.Format
    -- TODO: handle different content types
    go ["events"] GET = withDatabase db $ \conn -> do
        eSelect <- Control.Exception.try $ do
            select <- prepare conn
                "SELECT * from events ORDER BY ch, sesId, monoTime ASC;"
            _ <- execute select []
            return select

        case eSelect of
            Left (_e :: SomeException) -> do
                logM NOTICE $ "database error"
                respond $ responseLBS
                    status503
                    [("Content-Type", "text/plain")]
                    "database error"
            Right select -> do
                let action = respond $ responseStream
                        status200
                        [("Content-Type", "text/plain")]
                        $ \write flush -> do
                            runStream $
                                toProducer select
                                >-> Encodings.toByteString fmt
                                >-> sender write
                            flush
                            logM DEBUG "closing database"
                            disconnect conn
                    cleanup = do
                        logM NOTICE "streaming error"
                        logM DEBUG "closing database"
                        disconnect conn
                        respond $ responseLBS
                            status503
                            [("Content-Type", "text/plain")]
                            "streaming error"
                onException action cleanup
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

        writer :: ConnWrapper
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

