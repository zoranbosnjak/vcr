{-# LANGUAGE OverloadedStrings #-}

module CmdRecorder (cmdRecorder) where

import Control.Exception hiding (throw, assert)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Trans
import Data.Maybe
import qualified Data.UUID as U
import Data.UUID.V4
import Network.HTTP.Simple
import Network.Multicast
import Network.Socket
import Network.Socket.ByteString as NB
import Options.Applicative
import System.Clock
import System.Log.Logger (Priority(..))

import Action
import Buffer
import Event
import IO

{- TODO:
    - Stdin, delimit char or format
    - TCP server input
    - TCP client input
    - check UDP IPv6 unicast/multicast "[ipv6]" instead of "ip"

    - Stdout, delimit or format
    - check ipv6 http output

    - ko je connection vzpostavljen,
      naj se buffer prazni postopoma, ni nujno vse naenkrat
-}

cmdRecorder :: ParserInfo (Action ())
cmdRecorder = info (helper <*> (runCmd <$> options))
    (progDesc "recorder from various sources")

data Options = Options
    { optIdent  :: Maybe String
    , optChanel :: String
    , optArch :: Bool
    , optInput :: Input
    , optOutput :: Output
    , optBufferSize :: Thrashold
    , optBatchSize :: Thrashold
    -- retry http connect interval
    -- http response timeout
    } deriving (Show, Eq)

data Input
    = IUnicast Ip Port
    | IMulticast Ip Port LocalIp
    deriving (Show, Eq)

data Output
    = OHttp Ip Port
    | OStdout
    deriving (Show, Eq)

buffer :: Parser Thrashold
buffer = Thrashold
    <$> optional (option auto
        (long "maxEvents" <> help "maximum number of events before drop"))
    <*> optional (option kiloMega
        (long "maxBytes" <> help "maximum size in bytes before drop"))
    <*> optional (option auto
        (long "maxSeconds" <> help "maximum event age in seconds before drop"))

batch :: Parser Thrashold
batch = Thrashold
    <$> optional (option auto
        (long "batchEvents" <> help "wait for number of events before send"))
    <*> optional (option auto
        (long "batchBytes" <> help "wait for size in bytes before send"))
    <*> optional (option auto
        (long "batchSeconds" <> help "wait seconds before send"))

options :: Parser Options
options = Options
    <$> (optional $ strOption
            (long "ident" <> metavar "IDENT" <> help "recorder identifier"))
    <*> strOption
            (long "chanel" <> metavar "CH" <> help "chanel identifier")
    <*> switch (long "arch" <> help "set archive bit")
    <*> inputParse
    <*> outputParse
    <*> buffer
    <*> batch

inputParse :: Parser Input
inputParse = subparser $
    command "input" (info (helper <*> level2) (progDesc "input definition"))
  where
    level2 = subparser
        ( command "unicast" (info (helper <*> unicast) idm)
       <> command "multicast" (info multicast idm)
        )
    unicast = IUnicast
        <$> argument str (metavar "IP" <> help "IP address")
        <*> argument str (metavar "PORT" <> help "UDP port number")
    multicast = IMulticast
        <$> argument str (metavar "IP" <> help "IP address")
        <*> argument str (metavar "PORT" <> help "UDP port number")
        <*> argument str (metavar "LOCAL" <> help "local IP address")

outputParse :: Parser Output
outputParse = subparser $
    command "output" (info (helper <*> level2) (progDesc "output definition"))
  where
    level2 = subparser
        ( command "http" (info (helper <*> http) idm)
       <> command "stdout" (info (pure OStdout) idm)
        )
    http = OHttp
        <$> argument str (metavar "IP" <> help "IP address")
        <*> argument str (metavar "PORT" <> help "port number")

runCmd :: Options -> Action ()
runCmd opts = do
    logM "init" INFO $ show opts
    assert (anyLimit $ optBufferSize opts)
        "Some limit in buffer size is required."

    assert (anyLimit $ optBatchSize opts)
        "Some limit in batch size is required."

    sessionId <- liftIO nextRandom >>= return . U.toString

    recorderId <- do
        -- TODO... get hostname
        return $ fromMaybe "noident" $ optIdent opts

    buf <- liftIO $ atomically bufferNew

    input <- startInput
        (optInput opts)
        buf
        (SessionId sessionId)
        (SourceId recorderId)
        (Chanel $ optChanel opts)
        (optArch opts)
        (optBufferSize opts)

    tick <- liftIO (now >>= \(_,t) -> newTVarIO t)
    ticker <- liftIO $ async $ forever $ do
        threadDelaySec 1
        (_, t) <- now
        atomically $ writeTVar tick t

    output <- startOutput (optOutput opts) buf (optBatchSize opts) tick

    -- all threads shall remain running
    _ <- liftIO $ waitAnyCatchCancel $ [input, ticker, output]
    throw "process terminated"

startInput :: Input -> Buffer -> SessionId -> SourceId -> Chanel -> Bool
    -> Thrashold -> Action (Async a)
startInput i buf sessionId recorderId ch arch th = do
    let tell prio msg = logM (show i) prio msg
    tell INFO "starting"
    let (ip,port,mclocal) = case i of
            IUnicast ip' port' -> (ip', port', Nothing)
            IMulticast mcast' port' local' -> (mcast', port', Just local')

    sock <- liftIO $ do
        (serveraddr:_) <- getAddrInfo
            (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
            (Just ip)
            (Just port)
        sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

        case mclocal of
            Nothing -> return ()
            Just _ -> do
                setSocketOption sock ReuseAddr 1
                addMembership sock ip mclocal

        bind sock (addrAddress serveraddr)
        return sock

    liftIO $ async $ forever $ do
        (datagram, _addr) <- NB.recvFrom sock (2^(16::Int))
        ts <- now
        let evt = createEvent ts datagram
        notAppended <- atomically $ bufferAppend th buf [evt]
        runAction $ case notAppended of
            [] -> tell DEBUG $ show evt
            [x] -> tell NOTICE $ "error appending: " ++ show x
            _ -> tell ERROR $ "internal error: unexpected events"

  where
    createEvent (t1,t2) datagram = Event
        { eChanel = ch
        , eSourceId = recorderId
        , eUtcTime = t1
        , eMonotonicTime = t2
        , eArchive = arch
        , eSessionId = sessionId
        , eValue = datagram
        }

startOutput :: Output -> Buffer -> Thrashold -> TVar TimeSpec
    -> Action (Async a)
startOutput o buf th tick = do
    tell INFO "starting"
    liftIO $ async $ forever $ do
        msgs <- liftIO $ atomically $ bufferRead th buf tick
        case o of

            -- send data over http
            OHttp ip port -> do

                request' <- parseRequest $
                    "PUT http://"++ip++":"++port++"/events/json"
                let request = setRequestBodyJSON msgs $ request'
                    retryWith s = do
                        tellIO NOTICE s
                        threadDelaySec 3
                        process
                    process = do
                        eResponse <- try (httpLBS request)
                        case eResponse of
                            Left (SomeException _e) ->
                                retryWith "Unable to connect."
                            Right resp -> do
                                case getResponseStatusCode resp of
                                    200 -> tellIO DEBUG "Request processed."
                                    _ -> do retryWith $ show resp
                process

            -- print to stdout
            OStdout -> mapM_ print msgs

  where
    tell prio msg = logM (show o) prio msg
    tellIO a b = (runAction $ tell a b) >>= \_ -> return ()

threadDelaySec :: Double -> IO ()
threadDelaySec = threadDelay . round . (1000000*)

