{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdServe (cmdServe) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Data.Aeson
import Data.List
import Database.HDBC as DB
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Options.Applicative
import Web.Scotty as W
import System.Clock
import System.Log.Logger (Priority(..))

import Action
import Event
import IO

-- TODO: get all about Event from the module, eg. toNanosecs

cmdServe :: ParserInfo (Action ())
cmdServe = info (helper <*> (runCmd <$> CmdServe.options))
    (progDesc "server part of the recorder")

type RequestedReplicas = Int
type ActualReplicas = Int
data ConnStatus = ConnAlive | ConnDead

data Options = Options
    { optServe :: [Server]
    , optStore :: [Store]
    , optReplicas :: RequestedReplicas
    } deriving (Eq, Show)

data Server
    = ServerHttp Ip Int
    deriving (Eq, Show)

data Store
    = StoreSQLite3 FilePath
    | StorePostgreSQL String
    -- | StoreRemote Http   -- permanent store to a remote server (cascade)
    -- = StoreMemory Integer   -- store up to N events in memory (for test)
    -- | StoreSql Address KeepDays -- permanent store
    deriving (Eq, Show)

options :: Parser CmdServe.Options
options = CmdServe.Options
    <$> some server
    <*> some store
    <*> option auto
        (short 'n' <> long "replicas"
            <> help "requested number of replicas before acknowledge write"
            <> value 1)

server :: Parser Server
server = subparser $
    command "server" (info (helper <*> level2) (progDesc "server definition"))
  where
    level2 = subparser $
        command "http" (info (helper <*> http) idm)
    http = ServerHttp
        <$> argument str (metavar "IP" <> help "IP address")
        <*> argument auto (metavar "PORT" <> help "UDP port number")

store :: Parser Store
store = subparser $
    command "store" (info (helper <*> level2) (progDesc "store definition"))
  where
    level2 = subparser
        ( command "sqlite3" (info (helper <*> sqlite3) idm)
       <> command "postgres" (info (helper <*> postgres) idm)
        )
    sqlite3 = StoreSQLite3
        <$> argument str (metavar "PATH" <> help "File path.")
    postgres = StorePostgreSQL
        <$> argument str (metavar "STRING" <> help "Connection string.")

runCmd :: CmdServe.Options -> Action ()
runCmd opts = do
    logM "init" INFO $ show opts
    tid <- liftIO $ myThreadId

    assert (optReplicas opts <= length (optStore opts))
        "number of requested replicas must be <= available database connections"

    -- connect to databases, (re)create tables
    connections <- forM (optStore opts) $ \i -> do
        conn <- case i of
            StoreSQLite3 path -> liftIO $ connectSqlite3 path
            StorePostgreSQL _s -> undefined
        _ <- lift $ createTables conn
        return conn

    -- start servers
    servers <- forM (optServe opts) $ \i -> liftIO $ async $ case i of
        ServerHttp ip port ->
            serveHttp tid ip port connections (optReplicas opts)

    -- all threads shall remain running
    _ <- liftIO $ waitAnyCatchCancel servers
    throw "process terminated"

-- TODO: runSql wrapper, check and return connection status

-- | Create database tables.
createTables :: (IConnection conn) => conn -> IO ConnStatus
createTables conn = do
    _ <- DB.run conn (unwords
        [ "CREATE TABLE IF NOT EXISTS events"
        , "( hash VARCHAR(255) NOT NULL PRIMARY KEY ON CONFLICT REPLACE"
        , ", ch VARCHAR(255)"
        , ", srcId VARCHAR(255)"
        , ", utcTime DATETIME"
        , ", sesId VARCHAR(255)"
        , ", monoTime BIGINT"
        , ", value BLOB"
        , ")"
        ]) []
    commit conn
    return ConnAlive

-- | Deposit events to a database.
deposit :: (IConnection conn) => conn -> [Event] -> IO ConnStatus
deposit conn events = do
    mapM_ saveEvent events
    commit conn
    return ConnAlive
  where
    saveEvent e = DB.run conn "INSERT INTO events VALUES (?,?,?,?,?,?,?)"
        [ toSql $ show $ hash e
        , toSql $ show $ eChanel e
        , toSql $ show $ eSourceId e
        , toSql $ eUtcTime e
        , toSql $ show $ eSessionId e
        , toSql $ toNanoSecs $ eBootTime e
        , toSql $ eValue e
        ]

scOpts :: ThreadId -> Int -> W.Options
scOpts _parent port =
    let change = (setPort port) -- TODO: . (setOnException quit)
        {-
        quit req e = do
            _ <- runAction $ logM "web" ERROR $ show e
            killThread parent
        -}
    in W.Options { verbose = 0 , settings = change defaultSettings }

serveHttp :: (IConnection c) =>
    ThreadId -> Ip -> RequestedReplicas -> [c] -> Int -> IO ()
serveHttp parent _ip port conns replicas = scottyOpts (scOpts parent port) $ do
    let on uri method act = addroute method uri $ do
            -- this is required for testing
            addHeader "Access-Control-Allow-Origin" "*"
            eval <- runAction act
            (rc, obj, level, s) <- case eval of
                Left (rc, obj) -> do
                    return (rc, obj, NOTICE, "error: ")
                Right (rc, obj) -> return (rc, obj, DEBUG, "")
            status rc
            msg  <- do
                req <- request
                p <- params
                body' <- body
                return $ s
                    ++ "request: " ++ show req
                    ++ ", params: " ++ show p
                    ++ ", body: " ++ show body'
                    ++ " -> "
                    ++ "return code: " ++ show rc
            _ <- lift $ runAction $ logM "web" level msg
            obj

        -- run action or throw error
        act .! e = do
            mval <- lift act
            case mval of
                Nothing -> throwError e
                Just val -> return val

        onHead path = on path HEAD
        onGet path = on path GET
        onPut path = on path PUT
        onDelete path = on path DELETE

        ok = (status200, W.text "OK")
        noDecode = (status400, W.text "unable to decode body")
        --noResource = (status404, W.text "not found")

    -- quick response check
    "/ping" `onGet` return (status200, W.text "pong")

    -- simulate processing delay
    "/delay/:d" `onGet` do
        let threadDelaySec = threadDelay . round . (1000000*)

        dt :: Double <- lift $ param "d"
        liftIO $ threadDelaySec dt
        return ok

    -- /events methods

    "/events" `onHead` do
        undefined

    "/events" `onGet` do
        undefined

    -- TODO: handle different content types
    "/events" `onPut` do
        events :: [Event] <- (fmap decode body) .! noDecode
        n <- liftIO $ safeDeposit conns replicas events
        case n >= replicas of
            True -> return ok
            False -> return (status503,
                W.text "unable to store to all requested replicas")

    "/events" `onDelete` do
        undefined

-- try to store events to a number of connections
-- This function returns actual number of replicas,
-- that all given events are stored.
-- distribute connections randomly, where the seed is
-- the chanel id of the event, so that the same chanel data
-- tend to go to the same replica
--
-- TODO: run in parallel (async), starting at "requested" instances,
--  then try one more replica when detected a failure on one connection.
--
safeDeposit :: (IConnection conn) =>
    [conn] -> RequestedReplicas -> [Event] -> IO ActualReplicas
safeDeposit conns requested evts =
    process requested distinctChanels enumeratedConnections
  where

    distinctChanels = nub $ map eChanel evts

    enumeratedConnections = zip [(1::Int)..] conns

    permutate lst _seed = lst -- TODO

    -- process all chanels
    process n [] _ = return n -- all done, no more chanels
    process _ _ [] = return 0 -- not able, no more active connections
    process n (chanel:xs) conns' = do
        let lst = [e | e <- evts, eChanel e == chanel]
            conns'' = permutate conns' chanel
        deadConns <- depositMany requested [] conns'' lst
        let nextConns = [(i,c) | (i,c) <- conns'', i `notElem` deadConns]
            nextN = min n $ length nextConns
        process nextN xs nextConns

    -- deposit events to n connections, return list of dead connections
    depositMany n failed conns' lst
        | n <= 0 || null conns' = return failed
        | otherwise = do
            let ((ix,conn),rest) = (head conns', tail conns')
            st <- deposit conn lst
            case st of
                ConnAlive -> depositMany (n-1) failed conns' lst
                ConnDead -> depositMany n (ix:failed) rest lst

