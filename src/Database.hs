
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
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception
import qualified Data.HashMap.Strict as HMS
--import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.Simple as PG
import           Data.String (fromString)
import           Data.Sequence as DS

import qualified Common as C
import           Process
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
    -- , "CREATE INDEX ON events (ch DESC)"
    -- , "CREATE INDEX ON "events"(srcId DESC)"
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

data LoopEvent a
    = Input (Message a)
    | BufferOverflow [a]
    | SenderFinished [a]

-- | Database writer.
databaseWriter :: (Int,Double) -> Int -> ([Event.Event] -> IO ()) -> Db
    -> Consumer Event.Event

-- postgres database writer
databaseWriter (sendAt,sendTime) m dropAct (DbPostgreSQL connStr connectTime) =
  Consumer $ \consume -> do
    when (dropAt > (maxBound `div` 2)) $ fail "database buffer too big"
    stage1 consume DS.empty

  where

    sendTimer = registerDelay $ round (sendTime * 1000 * 1000)

    inChunks act s = case DS.null s of
        True -> return ()
        False -> do
            let (a,b) = DS.splitAt sendAt s
            _ <- act $ toList a
            inChunks act b

    dropEvents = inChunks dropAct

    writeEvents conn = inChunks $ \events -> do
        _ <- PG.withTransaction conn $ PG.executeMany conn insertString events
        return ()

    done s = writeToDb `onException` dropEvents s where
        writeToDb = do
            conn <- connectDb
            writeEvents conn s

    appendThen buf msg act = do
        let buf' = buf |> msg
        case DS.length buf' >= dropAt of
            False -> act buf'
            True -> do
                dropEvents buf'
                act DS.empty

    dropAt = m*sendAt

    insertString = fromString
        "INSERT INTO events VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING"

    connectDb = PG.connectPostgreSQL $ fromString connStr

    -- try to connect, while consuming data
    stage1 consume s = do
        a <- async $ fix $ \reconnect -> do
            C.logM C.INFO $ "(re)connecting to database " ++ show connStr
            try connectDb >>= \case
                Right conn -> return conn
                Left (_e :: IOException) -> do
                    C.threadDelaySec connectTime
                    reconnect
        loop a s
      where
        loop a buf = do
            rv <- atomically (fmap Left consume `orElse` fmap Right (waitSTM a))
            case rv of
                Left EndOfData -> cancel a >> done buf
                Left (Message msg) -> appendThen buf msg (loop a)
                Right conn -> do
                    C.logM C.INFO "database connection established"
                    pending <- case DS.null buf of
                        True -> return Nothing
                        False -> do
                            -- fake send timer has expired
                            d <- newTVarIO True
                            return $ Just (Left d)
                    stage2 consume conn pending buf

    -- with active database connection
    stage2 consume conn pending buf = case pending of

        -- empty buffer, no pending operation
        Nothing -> atomically consume >>= \case
            EndOfData -> done buf
            Message msg -> do
                d <- sendTimer
                appendThen buf msg (stage2 consume conn (Just $ Left d))

        -- some data in buffer to be sent when delay expires or cnt >= sendAt
        Just (Left d) -> do
            let expired = readTVar d >>= check
            atomically (fmap Left consume `orElse` fmap Right expired) >>= \case
                Left EndOfData -> done buf
                Left (Message msg) -> appendThen buf msg $ \buf' -> do
                    case DS.length buf' >= sendAt of
                        False -> stage2 consume conn (Just $ Left d) buf'
                        True -> do
                            a <- async $ try $ writeEvents conn buf'
                            stage2 consume conn (Just $ Right (a, buf)) DS.empty
                Right () -> do
                    a <- async $ try $ writeEvents conn buf
                    stage2 consume conn (Just $ Right (a, buf)) DS.empty

        -- sending of buf1 in progress
        Just (Right (a,buf1)) -> do
            let task = waitSTM a
            atomically (fmap Left consume `orElse` fmap Right task) >>= \case
                Left EndOfData -> do
                    cancel a
                    done $ buf1 >< buf
                Left (Message msg) -> appendThen buf msg
                    (stage2 consume conn (Just $ Right (a,buf1)))
                Right (Left (e :: IOException)) -> do
                    C.logM C.NOTICE $ "database error: " ++ show e
                    stage1 consume $ buf1 >< buf
                Right (Right _)
                    | DS.null buf -> stage2 consume conn Nothing buf
                    | DS.length buf >= sendAt -> do
                        b <- async $ try $ writeEvents conn buf
                        stage2 consume conn (Just $ Right (b, buf)) DS.empty
                    | otherwise -> do
                        d <- sendTimer
                        stage2 consume conn (Just $ Left d) buf

databaseWriter _ _ _ (DbSQLite3 _filename) = undefined

