
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Foldable (toList)
import           GHC.Generics (Generic)
import           Options.Applicative as Opt hiding (action)
import           Data.Bool
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Control.Concurrent.STM
import           Control.Concurrent.Async as Async
import qualified Control.Exception as Ex
import qualified Data.HashMap.Strict as HMS
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.SQLite.Simple as SL
import           Data.String (fromString)
import           Data.Sequence as DS
import qualified System.Clock as Clk
import qualified Data.Text as Text

import           Pipes
import qualified Pipes.Safe as PS
import qualified Pipes.Concurrent as PC

import qualified Common as C
import qualified Event

type RetryTimeout = Double

data Status
    = Status Int Int      -- (current buffer level, drop threshold)
    deriving (Generic, Eq, Show)

instance ToJSON Status
instance FromJSON Status


data Buffer = Buffer
    { bufThreshold  :: Int
    , bufTimeout    :: Double
    } deriving (Generic, Eq, Show)

instance ToJSON Buffer
instance FromJSON Buffer

class IConnection conn where
    writeEvents :: conn -> [Event.Event] -> IO ()
    execute_ :: conn -> String -> IO ()
    fold_ :: conn -> String -> a -> (a -> Event.Event -> IO a) -> IO a

    forEach_ :: conn -> String -> (Event.Event -> IO ()) -> IO ()
    forEach_ conn s f = fold_ conn s () (const f)

data Connection
    = ConnPostgres PG.Connection
    | ConnSqlite SL.Connection

instance IConnection Connection where
    writeEvents (ConnPostgres conn) events = do
        _ <- PG.withTransaction conn $
            PG.executeMany conn (fromString insertString) events
        return ()
      where
        insertString =
            "INSERT INTO events VALUES"
            ++ " (?,?,?,?,?,?,?,?,?,?)"
            ++ " ON CONFLICT DO NOTHING"
    writeEvents (ConnSqlite conn) events = do
        _ <- SL.withTransaction conn $
            SL.executeMany conn (fromString insertString) events
        return ()
      where
        insertString =
            "INSERT OR IGNORE INTO events VALUES"
            ++ " (?,?,?,?,?,?,?,?,?,?)"
    execute_ (ConnPostgres c) s = PG.execute_ c (fromString s) >> return ()
    execute_ (ConnSqlite c) s = SL.execute_ c (fromString s)
    fold_ (ConnPostgres c) s = PG.fold_ c (fromString s)
    fold_ (ConnSqlite c) s = SL.fold_ c (fromString s)

data Db
    = DbPostgreSQL String RetryTimeout -- postgresql (connection string)
    | DbSQLite3 FilePath    -- sqlite3 (file)
    deriving (Generic, Eq, Show)

instance ToJSON Db where
    toJSON (DbPostgreSQL connStr timeout) = object
        [ "type" .= String "postgres"
        , "connection" .= connStr
        , "timeout" .= timeout
        ]
    toJSON (DbSQLite3 path) = object
        [ "type" .= String "sqlite"
        , "path" .= path
        ]

instance FromJSON Db where
    parseJSON = withObject "Db" $ \v -> case HMS.lookup "type" v of
        Just "postgres" -> DbPostgreSQL
            <$> v .: "connection"
            <*> v .: "timeout"
        Just "sqlite" -> DbSQLite3
            <$> v .: "path"
        _ -> typeMismatch "Db" $ String "wrong type"

-- | Connect to database
connect :: Db -> IO Connection
connect = \case
    DbPostgreSQL connStr _timeout -> ConnPostgres <$>
        (PG.connectPostgreSQL $ fromString connStr)
    DbSQLite3 fn -> ConnSqlite <$> (SL.open fn)

databaseConnectionOptions :: Opt.Parser Db
databaseConnectionOptions =
    ( C.subparserCmd "postgres ..." $ Opt.command "postgres" $ Opt.info
            ((DbPostgreSQL <$> parsePostgres <*> retryTimeout) <**> Opt.helper)
            (Opt.progDesc "connect to postgres database")
        )
   <|> ( C.subparserCmd "sqlite ..." $ Opt.command "sqlite" $ Opt.info
            (fmap DbSQLite3 parseSqlite <**> Opt.helper)
            (Opt.progDesc "connect to sqlite database")
        )

parsePostgres :: Opt.Parser String
parsePostgres = Opt.strOption
    (Opt.long "connect"
   <> Opt.metavar "CONNECTION"
   <> Opt.help "connection string"
    )

parseSqlite :: Opt.Parser FilePath
parseSqlite = Opt.strOption
    (Opt.long "filename"
   <> Opt.metavar "PATH"
   <> Opt.help "database file"
    )

retryTimeout :: Opt.Parser RetryTimeout
retryTimeout = Opt.option Opt.auto
    ( Opt.long "retry"
   <> Opt.metavar "SECONDS"
   <> Opt.help "Retry timeout"
   <> Opt.value 10
   <> Opt.showDefault
    )

-- | Prepare database tables.
prepareDatabase :: Db -> IO ()
prepareDatabase db = do
    conn <- connect db
    execute_ conn createTable
    mapM_ (execute_ conn) indices
  where
    createTable = unwords
        [ "CREATE TABLE events"
        , "( ch VARCHAR(255) NOT NULL"
        , ",srcId VARCHAR(255)"
        , ",utcTime TIMESTAMP WITH TIME ZONE NOT NULL"
        , ",utcTimePico BIGINT NOT NULL"
        , ",monoTimeSec BIGINT NOT NULL"
        , ",monoTimeNSec BIGINT NOT NULL"
        , ",sesId VARCHAR(255) NOT NULL"
        , ",trkId VARCHAR(255) NOT NULL"
        , ",seqNum INT NOT NULL"
        , ",value BYTEA"
        , ",UNIQUE (monoTimeSec, monoTimeNSec, trkId, seqNum)"
        , ")"
        ]
    indices :: [String]
    indices =
        [ "CREATE INDEX iUtc ON events (utcTime DESC)"
        , "CREATE INDEX iChUtc ON events (ch, utcTime DESC)"
        , "CREATE INDEX iTrkMono ON events (trkId, monoTimeSec, monoTimeNSec)"
        ]

-- | Format UTC time for SQL (YYYY-MM-DD HH:MM:SS.SSS)
fmtTime :: Show a => a -> String
fmtTime t = "'" ++ show t ++ "'"

-- | Database reader task (do not attempt to reconnect).
databaseReaderTask ::
    Db
    -> Maybe (Event.UtcTime)
    -> Maybe (Event.UtcTime)
    -> [Event.Channel]
    -> [Event.SourceId]
    -> Producer Event.Event (PS.SafeT IO) ()
databaseReaderTask db mStart mEnd channels sources =
    PS.bracket prepare finalize action
  where
    prepare = do
        C.logM C.DEBUG $ selectEvents
        (output, input, seal) <- PC.spawn' $ PC.bounded 1
        conn <- connect db
        a <- async $ do
            rv <- Ex.try $ forEach_ conn selectEvents $ \event -> do
                _ <- atomically . PC.send output $ event
                return ()
            case rv of
                Right () -> return ()
                Left (e :: Ex.IOException) -> C.logM C.NOTICE $ show e
            atomically seal
            PC.performGC
        return (input, a)

    finalize (_, a) = cancel a

    action (input,_) = PC.fromInput input

    selectEvents =
        "SELECT * FROM events WHERE NULL IS NULL"
        ++ maybe "" (\x -> " AND utcTime >= " ++ fmtTime x) mStart
        ++ maybe "" (\x -> " AND utcTime < " ++ fmtTime x) mEnd
        ++ maybe "" (\x -> " AND " ++ x) chList
        ++ maybe "" (\x -> " AND " ++ x) recList
        ++ " ORDER BY monoTimeSec, monoTimeNSec, ch, sesId ASC"
    chList = do
        guard $ channels /= []
        let lst = Prelude.map
                (\(Event.Channel ch) -> "'" ++ Text.unpack ch ++ "'") channels
        return $ "ch in (" ++ foldl1 (\a b -> a ++ "," ++ b) lst ++ ")"
    recList = do
        guard $ sources /= []
        let lst = Prelude.map
                (\(Event.SourceId src) -> "'" ++ Text.unpack src ++ "'") sources
        return $ "srcId in (" ++ foldl1 (\a b -> a ++ "," ++ b) lst ++ ")"

-- | Database writer task (do not attempt to reconnect).
databaseWriterTask :: Int -> Db -> Consumer Event.Event (PS.SafeT IO) c
databaseWriterTask thSend db = PS.bracket prepare finalize action where
    prepare = (,) <$> connect db <*> newTVarIO DS.empty
    finalize (conn, buf) = do
        events <- toList <$> (atomically $ readTVar buf)
        writeEvents conn events
    action (conn, buf) = forever $ do
        event <- await
        mEvents <- liftIO $ atomically $ do
            events <- readTVar buf
            let events' = events |> event
            case DS.length events' >= thSend of
                False -> writeTVar buf events' >> return Nothing
                True -> writeTVar buf DS.empty >> return (Just events')
        case mEvents of
            Nothing -> return ()
            Just events -> liftIO $ writeEvents conn $ toList events

-- | Database writer process.
databaseWriterProcess :: (Status -> STM ()) -> Buffer -> Int
    -> (DS.Seq Event.Event -> STM ()) -> Db
    -> Consumer Event.Event (PS.SafeT IO) c
databaseWriterProcess setStat th thDrop dropEvents db
    | thDrop > (maxBound `div` 2) = fail "drop threshold too big"
    | thDrop <= thSend = fail "drop threshond <= send threshold"
    | otherwise = PS.bracket prepare finalize consumer
  where
    thSend = bufThreshold th
    dt = bufTimeout th
    prepare = do
        atomically $ setStat $ Status 0 thDrop
        bufV <- newTVarIO DS.empty  -- buffer variable
        sealed <- newTVarIO False   -- a flag to stop processing
        a <- async (withoutConnection bufV sealed)
        return (bufV, sealed, a)

    finalize (bufV, sealed, a) = do
        atomically $ writeTVar sealed True
        _ <- wait a
        buf <- atomically $ readTVar bufV
        when (not $ DS.null buf) $ tryConnect >>= \case
            Nothing -> atomically $ dropEvents (snd <$> buf)
            Just conn -> do
                let events = snd <$> toList buf
                Ex.try (writeEvents conn events) >>= \case
                    Right () -> return ()
                    Left (_e :: Ex.IOException) -> do
                        atomically $ dropEvents (snd <$> buf)

    -- try to reconnect
    withoutConnection bufV sealed = fix $ \reconnect -> do
        C.logM C.INFO $ reconnecting db
        race finish tryConnect >>= \case
            Left _ -> return ()
            Right Nothing -> sleepReconnect db sealed >> reconnect
            Right (Just conn) ->  do
                C.logM C.INFO $ connected db
                withConnection conn bufV sealed
      where
        finish = atomically $ readTVar sealed >>= bool retry (return ())

    tryConnect = Ex.try (connect db) >>= \case
        Right conn -> return (Just conn)
        Left (e :: Ex.IOException) -> case db of
            DbPostgreSQL _ _ -> return Nothing
            DbSQLite3 _ -> fail $ show e  -- sqlite failure is a problem

    -- send events to database in chunks
    withConnection conn bufV sealed = fix $ \loop -> do
        timeout <- do
            -- check first element, to calculate timeout
            mEl <- atomically $ (DS.lookup 0 <$> readTVar bufV)
            case mEl of
                Nothing -> registerDelay $ round $ dt * 1000 * 1000
                Just (t1', _) -> do
                    t <- Clk.toNanoSecs <$> Clk.getTime Clk.Boottime
                    let t1 = Clk.toNanoSecs t1'
                        t2 = t1 + round (dt * 1000 * 1000 * 1000)
                        d = t2 - t
                    case d <= 0 of  -- expire now or setup timer
                        True -> newTVarIO True
                        False -> registerDelay $ fromInteger (d `div` 1000)
        mChunk <- atomically $ readTVar sealed >>= \case
            True -> return Nothing
            False -> readTVar timeout >>= \case
                False -> retry
                True -> do
                    buf <- readTVar bufV
                    let (a,b) = DS.splitAt thSend buf
                    when (DS.null a) retry
                    writeTVar bufV b
                    return $ Just a
        case mChunk of
            Nothing -> return ()
            Just chunk -> do
                let events = snd <$> toList chunk
                Ex.try (writeEvents conn events) >>= \case
                    Right () -> loop
                    Left (e :: Ex.IOException) -> do
                        atomically $ modifyTVar bufV (chunk ><)
                        C.logM C.NOTICE $ "database exception " ++ show e
                        sleepReconnect db sealed
                        withoutConnection bufV sealed

    -- consume data to buffer, add timestamp to each message
    -- drop events on buffer overflow
    consumer (bufV, _, _) = forever $ do
        msg <- await
        t <- liftIO $ Clk.getTime Clk.Boottime
        liftIO $ atomically $ do
            buf <- readTVar bufV
            let buf' = buf |> (t,msg)
            case (DS.length buf') >= thDrop of
                False -> do
                    writeTVar bufV buf'
                    setStat $ Status (DS.length buf') thDrop
                True -> do
                    dropEvents (snd <$> buf')
                    writeTVar bufV DS.empty
                    setStat $ Status 0 thDrop

    sleepReconnect (DbPostgreSQL _connStr timeout) sealed = race_
        (C.threadDelaySec timeout)
        (atomically (readTVar sealed >>= bool retry (return ())))
    sleepReconnect (DbSQLite3 _fn) _ = return ()

    reconnecting (DbPostgreSQL connStr _) =
        "(re)connecting to database " ++ show connStr
    reconnecting (DbSQLite3 fn) =
        "(re)connecting to database file " ++ show fn

    connected (DbPostgreSQL connStr _) =
        "connected to database " ++ show connStr
    connected (DbSQLite3 fn) =
        "connected to database file " ++ show fn

