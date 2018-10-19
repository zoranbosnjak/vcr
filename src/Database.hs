
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
import           Data.String (fromString)
import           Data.Sequence as DS
import qualified System.Clock as Clk

import qualified Common as C
import           Streams
import qualified Event

type RetryTimeout = Double

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

indices :: [String]
indices =
    [ "CREATE INDEX ON events (utcTime DESC)"
    , "CREATE INDEX ON events (ch, utcTime DESC)"
    , "CREATE INDEX ON events (trkId, monoTimeSec, monoTimeNSec)"
    ]

-- | Prepare database tables.
prepareDatabase :: Db -> IO ()

prepareDatabase (DbPostgreSQL s _to) = do
    conn <- PG.connectPostgreSQL $ fromString s
    PG.execute_ conn (fromString createTable) >> return ()
    forM_ indices $ \createIndex -> do
        PG.execute_ conn (fromString createIndex) >> return ()
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
        , ",UNIQUE (ch, utcTime, sesId, monoTimeSec, monoTimeNSec)"
        , ")"
        ]

prepareDatabase (DbSQLite3 _filename) = undefined

-- | Database writer.
databaseWriter :: (Int,Double) -> Int -> ([Event.Event] -> IO ()) -> Db
    -> Consumer Event.Event c

-- postgres database writer
databaseWriter (thSend, dt) thDrop dropEvents
  (DbPostgreSQL connStr connectTime) = mkConsumer $ \consume -> do
    when (thDrop > (maxBound `div` 2)) $ fail "drop threshold too big"
    when (thDrop <= thSend) $ fail "drop threshond <= send threshold"
    bufV <- newTVarIO DS.empty  -- buffer variable
    connV <- newTVarIO Nothing  -- database connection variable
    withAsync (reader bufV consume) $ \rd ->
        withAsync (connector connV) $ \_ -> fix $ \loop -> do
            timeout <- do
                mEl <- DS.lookup 0 <$> atomically (readTVar bufV)
                case mEl of
                    Nothing -> newTVarIO False  -- empty buffer
                    Just (t1', _) -> do
                        t <- Clk.toNanoSecs <$> Clk.getTime Clk.Boottime
                        let t1 = Clk.toNanoSecs t1'
                            t2 = t1 + round (dt * 1000 * 1000 * 1000)
                            d = t2 - t
                        case d <= 0 of  -- expire now or setup timer
                            True -> newTVarIO True
                            False -> registerDelay $ fromInteger (d `div` 1000)
            join $ atomically $ getNext bufV timeout connV rd >>= \case
                -- send some events to database or put them back to buffer
                Right (Right (chunk, conn)) -> return $ do
                    let events = snd <$> toList chunk
                    try (writeEvents conn events) >>= \case
                        Right () -> return ()
                        Left (_e :: IOException) -> atomically $ do
                            modifyTVar bufV (chunk ><)
                            writeTVar connV Nothing
                    loop

                -- drop some events
                Right (Left chunk) -> return $ do
                    let events = snd <$> toList chunk
                    dropEvents events
                    loop

                -- end of data, empty buffer
                Left rv -> return (return rv)

  where
    connectDb = PG.connectPostgreSQL $ fromString connStr

    insertString = fromString
        "INSERT INTO events VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"

    writeEvents conn events = do
        _ <- PG.withTransaction conn $ PG.executeMany conn insertString events
        return ()

    getNext bufV timeout connV rd =
        fmap (Right . Left) dropSome    -- check this first to prevent overflow
        `orElse` fmap (Right . Right) sendSome
        `orElse` eof
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
        eof = do
            rv <- waitSTM rd
            buf <- readTVar bufV
            let (a,b) = DS.splitAt thSend buf
            writeTVar bufV b
            case DS.null a of
                True -> return $ Left rv    -- end of data, empty buffer
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

    -- try to (re)connect to the database (when disconnected)
    connector connV = fix $ \reconnect -> do
        _ <- atomically $ readTVar connV >>= \case
            Just _ -> retry     -- block until disconnected
            Nothing -> return ()
        C.logM C.INFO $ "(re)connecting to database " ++ show connStr
        try connectDb >>= \case
            Right conn -> do
                C.logM C.INFO $ "connected to database " ++ show connStr
                atomically $ writeTVar connV $ Just conn
            Left (_e :: IOException) -> C.threadDelaySec connectTime
        reconnect

databaseWriter _ _ _ (DbSQLite3 _filename) = undefined

