{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdServe (cmdServe) where

-- TODO: propagate exception in handlers

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Data.Aeson
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.HTTP.Types
--import Network.Wai
import Options.Applicative
import Web.Scotty as W
import System.Log.Logger (Priority(..))

import Action
import Event
import IO

cmdServe :: ParserInfo (Action ())
cmdServe = info (helper <*> (runCmd <$> CmdServe.options))
    (progDesc "server part of the recorder")

data Options = Options
    { optServe :: [Server]
    , optStore :: [Store]
    , optReplicas :: Int
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

    -- connect to databases
    connections <- forM (optStore opts) $ \i -> case i of
        StoreSQLite3 path -> liftIO $ connectSqlite3 path
        StorePostgreSQL _s -> undefined

    mapM_ (lift . createTables) connections

{-
    -- start servers
    servers <- forM (optServe opts) $ \i -> liftIO $ async $ case i of
        ServerHttp ip port -> serveHttp ip port

    -- all threads shall remain running
    _ <- liftIO $ waitAnyCatchCancel $ servers -- ++ ...
    throw "process terminated"
-}

createTables conn = do
    _ <- run conn (unlines
        [ "CREATE TABLE IF NOT EXISTS test"
        , "(id INTEGER NOT NULL, desc VARCHAR(80))"
        ]) []
    commit conn

{-
type RequestedReplicas = Int
type ActualReplicas = Int
type Success = Bool

serveHttp :: Ip -> Int -> IO ()
serveHttp _ip port = scotty port $ do
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

    "/ping" `onGet` return (status200, W.text "pong")

    "/delay/:d" `onGet` do
        let threadDelaySec = threadDelay . round . (1000000*)

        dt :: Double <- lift $ param "d"
        liftIO $ threadDelaySec dt
        return ok

    -- /events/* handling
    do
        "/events" `onHead` do
            undefined

        "/events" `onGet` do
            undefined

        "/events/json" `onPut` do
            val :: [Event] <- (fmap decode body) .! noDecode
            liftIO $ print val
            return ok

        "/events/bson" `onPut` undefined
        "/events/txt"  `onPut` undefined
        "/events/yaml"  `onPut` undefined

        "/events" `onDelete` do
            undefined

-- try to store events to a number of handlers
-- This function returns actual number of replicas,
-- that all given events are stored.
-- distribute handlers randomly, where the seed is
-- the chanel id of the event, so that the same chanel data
-- tend to go to the same replica
safeDeposit :: (Eq storeHandler) =>
    [storeHandler] -> [Event] -> RequestedReplicas -> IO ActualReplicas
safeDeposit handlers evts requested = process requested distinctChanels handlers
  where
    distinctChanels = nub $ map eChanel evts
    process n [] _ = return n -- all done, no more chanels
    process _ _ [] = return 0 -- no more active handlers
    process n (chanel:xs) handlers' = do
        let lst = filter (\e -> eChanel e == chanel) evts
            handlers'' = permutate handlers' chanel
        deadHandlers <- depositMany requested [] handlers'' lst
        let handlers''' = [h | h<-handlers', h `notElem` deadHandlers]
            nextN = min n $ length handlers'''
        process nextN xs handlers'''

    -- deposit events to n handlers, return list of dead handlers
    depositMany n dhs hs lst
        | n <= 0 || null hs = return dhs
        | otherwise = do
            let handler = head hs
                rest = tail hs
            ok <- deposit handler lst
            case ok of
                True -> depositMany (n-1) dhs hs lst
                False -> depositMany n (handler:dhs) rest lst

    permutate lst _seed = lst -- TODO

-- deposit events to a single handler
deposit :: storeHandler -> [Event] -> IO Success
deposit _handler _lst = undefined
-}

