{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdServer (cmdServer) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Data.Aeson
import Network.HTTP.Types
--import Network.Wai
import Options.Applicative
import Web.Scotty as W
import System.Log.Logger (Priority(..))

import Action
import Buffer
import Event
import IO

{- TODO:
    - propagate exception in handlers

-}

cmdServer :: ParserInfo (Action ())
cmdServer = info (helper <*> (runCmd <$> CmdServer.options))
    (progDesc "server part of the recorder")

data Options = Options
    { optServe :: [Server]
    , optStore :: [Store]
    -- , optReplicas :: Maybe Int
    -- , optKeepDays :: Maybe Double
    } deriving (Eq, Show)

data Server
    = ServerHttp Ip Int
    deriving (Eq, Show)

data Store
    = StoreStdout
    | StoreStderr
    -- | StoreSql Address KeepDays
    -- | StoreRemote Http
    deriving (Eq, Show)

options :: Parser CmdServer.Options
options = CmdServer.Options
    <$> some server
    <*> some store
    {-
    <*> (optional $ option auto
        (short 'n' <> long "replicas" <> help "number of replicas"))
    <*> (optional $ option auto
        (long "keepDays" <> help "keep replicas N days"))
    -}

server :: Parser Server
server = subparser $
    command "serve" (info (helper <*> level2) (progDesc "server definition"))
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
        ( command "stdout" (info (helper <*> (pure StoreStdout)) idm)
       <> command "stderr" (info (helper <*> (pure StoreStderr)) idm)
        )

runCmd :: CmdServer.Options -> Action ()
runCmd opts = do
    logM "init" INFO $ show opts

    -- TODO: check limits on buffer

    buf <- liftIO $ atomically bufferNew

    -- start servers
    servers <- forM (optServe opts) $ \i -> liftIO $ async $ case i of
        ServerHttp ip port -> serveHttp buf ip port

    -- start store handlers
    liftIO $ print $ optStore opts

    -- all threads shall remain running
    _ <- liftIO $ waitAnyCatchCancel $ servers -- ++ ...
    throw "process terminated"

serveHttp :: Buffer -> Ip -> Int -> IO ()
serveHttp _buf _ip port = scotty port $ do
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

