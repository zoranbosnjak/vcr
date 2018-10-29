
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
import           Control.Concurrent.Async
import           Control.Exception
import qualified Data.HashMap.Strict as HMS
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.SQLite.Simple as SL
import           Data.String (fromString)
import           Data.Sequence as DS
import qualified System.Clock as Clk
import qualified Data.Text as Text

import qualified Common as C
import           Streams
import qualified Event

type RetryTimeout = Double

data Status
    = Status Int Int      -- (current buffer level, drop threshold)
    deriving (Generic, Eq, Show)

instance ToJSON Status
instance FromJSON Status

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

-- | Database reader task.
databaseReaderTask :: Db
    -> Maybe (Event.UtcTime)
    -> Maybe (Event.UtcTime)
    -> [Event.Channel]
    -> [Event.SourceId]
    -> Producer Event.Event ()
databaseReaderTask db mStart mEnd channels sources =
  mkProducer $ \produce -> do
    conn <- connect db
    C.logM C.DEBUG $ selectEvents
    forEach_ conn selectEvents $ atomically . produce
  where
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

-- | Database writer task.
databaseWriterTask :: Int -> Db -> Consumer Event.Event c
databaseWriterTask thSend db =
  mkConsumer $ \consume -> do
    conn <- connect db
    loop consume conn []
  where
    loop consume conn events = atomically consume >>= \case
        EndOfData rv -> do
            writeEvents conn events
            return rv
        Message event -> do
            let events' = event:events
            case Prelude.length events' >= thSend of
                True -> do
                    writeEvents conn events'
                    loop consume conn []
                False ->
                    loop consume conn events'

-- | Database writer.
databaseWriter :: (Status -> STM ()) -> (Int,Double) -> Int
    -> ([Event.Event] -> IO ()) -> Db -> Consumer Event.Event c
databaseWriter setStat (thSend, dt) thDrop dropEvents db =
  mkConsumer $ \consume -> do
    when (thDrop > (maxBound `div` 2)) $ fail "drop threshold too big"
    when (thDrop <= thSend) $ fail "drop threshond <= send threshold"
    bufV <- newTVarIO DS.empty  -- buffer variable
    atomically $ setStat $ Status 0 thDrop
    connV <- newTVarIO Nothing  -- database connection variable
    tryConnect connV
    withAsync (reader bufV consume) $ \rd ->
        withAsync (connector connV) $ \_ -> fix $ \loop -> do
            timeout <- do
                mEl <- DS.lookup 0 <$> atomically (readTVar bufV)
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
            join $ atomically $ do
              buf <- readTVar bufV
              setStat $ Status (DS.length buf) thDrop
              getNext bufV timeout connV rd >>= \case
                -- send some events to database or put them back to buffer
                Right (Right (chunk, conn)) -> return $ do
                    let events = snd <$> toList chunk
                    try (writeEvents conn events) >>= \case
                        Right () -> return ()
                        Left (e :: PG.SqlError) -> do
                            C.logM C.NOTICE $ "database exception " ++ show e
                            atomically $ do
                                modifyTVar bufV (chunk ><)
                                writeTVar connV Nothing
                    loop

                -- drop some events
                Right (Left chunk) -> return $ do
                    let events = snd <$> toList chunk
                    dropEvents events
                    loop

                -- tick
                Left (Left ()) -> return loop

                -- end of data, empty buffer
                Left (Right rv) -> return (return rv)

  where

    getNext bufV timeout connV rd =
        fmap (Right . Left) dropSome    -- check this first to prevent overflow
        `orElse` fmap (Right . Right) sendSome
        `orElse` eof
        `orElse` tick
      where
        dropSome = atThreshold thDrop
        sendSome = do
            conn <- readTVar connV >>= maybe retry return -- requires connection
            values <- expired `orElse` atThreshold thSend
            return (values, conn)
        expired = do
            _ <- readTVar timeout >>= bool retry (return ())
            buf <- readTVar bufV
            let (a,b) = DS.splitAt thSend buf
            when (DS.null a) retry
            writeTVar bufV b
            return a
        atThreshold th = do
            buf <- readTVar bufV
            case DS.length buf >= th of
                False -> retry
                True -> do
                    let (a,b) = DS.splitAt thSend buf
                    writeTVar bufV b
                    return a
        tick = do
            _ <- readTVar timeout >>= bool retry (return ())
            return $ Left (Left ())
        eof = do
            rv <- waitSTM rd
            buf <- readTVar bufV
            let (a,b) = DS.splitAt thSend buf
            writeTVar bufV b
            case DS.null a of
                True -> return $ Left (Right rv)    -- end of data, empty buffer
                False -> readTVar connV >>= \case
                    Nothing -> return $ Right $ Left a
                    Just conn -> return $ Right $ Right (a, conn)

    -- consume data to buffer, add timestamp to each message
    reader bufV consume = fix $ \loop -> do
        x <- atomically consume
        t <- Clk.getTime Clk.Boottime
        case x of
            EndOfData rv -> return rv
            Message msg -> do
                atomically $ modifyTVar bufV (\val -> (val |> (t,msg)))
                loop

    reconnecting (DbPostgreSQL connStr _) =
        "(re)connecting to database " ++ show connStr
    reconnecting (DbSQLite3 fn) =
        "(re)connecting to database file " ++ show fn
    connected (DbPostgreSQL connStr _) =
        "connected to database " ++ show connStr
    connected (DbSQLite3 fn) =
        "connected to database file " ++ show fn

    tryConnect connV = do
        C.logM C.INFO $ reconnecting db
        try (connect db) >>= \case
            Right conn -> do
                C.logM C.INFO $ connected db
                atomically $ writeTVar connV $ Just conn
            Left (e :: IOException) -> case db of
                DbPostgreSQL _ _ -> return () --
                DbSQLite3 _ -> fail $ show e  -- fail in case of sqlite failure

    -- try to (re)connect to the database (when disconnected)
    connector connV = fix $ \reconnect -> do
        _ <- atomically $ readTVar connV >>= \case
            Just _ -> retry     -- block until disconnected
            Nothing -> return ()
        case db of
            DbPostgreSQL _ connectTime -> do
                C.threadDelaySec connectTime -- do not reconnect too fast
            DbSQLite3 _ -> return ()
        tryConnect connV
        reconnect

