{-# LANGUAGE OverloadedStrings #-}

module CmdRecord (cmdRecord) where

import Control.Exception hiding (throw, assert)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forM_)
import Control.Monad.Trans
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe
import Data.Monoid
import qualified Data.UUID as U
import Data.UUID.V4
import Network.HTTP.Simple
import Network.Multicast
import Network.Socket
import Network.Socket.ByteString as NB
import Options.Applicative
import System.Log.Logger (Priority(..))
import qualified System.IO
import qualified System.Posix.Syslog as SL

import Action
import Buffer
import Event
import IO

{- TODO:
    - check UDP IPv6 unicast/multicast "[ipv6]" instead of "ip"
    - check ipv6 http output
    - ko je connection vzpostavljen,
      naj se buffer prazni postopoma, ne nujno vse naenkrat
-}

cmdRecord :: ParserInfo (Action ())
cmdRecord = info (helper <*> (runCmd <$> options))
    (progDesc "event recorder")

data Options = Options
    { optIdent  :: Maybe String
    , optChannel :: String
    , optInput :: Input
    , optOutput :: Output
    , optBufferSize :: Thrashold
    , optBatchSize :: Thrashold
    , optDrop :: [Drop]
    -- retry http connect interval
    -- http response timeout
    } deriving (Show, Eq)

data Input
    = IUnicast Ip Port
    | IMulticast Ip Port LocalIp
    -- IStdin Format...
    -- TCPServer Ip Port Format...
    -- TCPClient Ip Port Format
    deriving (Show, Eq)

data Output
    = OFile FilePath EncodeFormat
    | OHttp Ip Port
    deriving (Show, Eq)

data Drop
    = DSyslog SL.Priority EncodeFormat
    | DFile FilePath EncodeFormat
    deriving (Show, Eq)

options :: Parser Options
options = Options
    <$> (optional $ strOption
            (long "ident" <> metavar "IDENT" <> help "recorder identifier"))
    <*> strOption
            (long "channel" <> metavar "CH" <> help "channel identifier")
    <*> inputParse
    <*> outputParse
    <*> bufferParse
    <*> batchParse
    <*> many dropParse

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
        ( command "file" (info (helper <*> file) idm)
       <> command "http" (info (helper <*> http) idm)
        )
    file = OFile
        <$> argument str (metavar "FILE"
            <> help "destination filename, '-' for stdout")
        <*> formatParse
    http = OHttp
        <$> argument str (metavar "IP" <> help "IP address")
        <*> argument str (metavar "PORT" <> help "port number")

formatParse :: Parser EncodeFormat
formatParse = pure EncShow -- TODO: decode all formating

bufferParse :: Parser Thrashold
bufferParse = thrashold
    <$> optional (option auto
        (long "maxEvents" <> help "maximum number of events before drop"))
    <*> optional (option kiloMega
        (long "maxBytes" <> help "maximum size in bytes before drop"))
    <*> optional (option auto
        (long "maxSeconds" <> help "maximum event age in seconds before drop"))

batchParse :: Parser Thrashold
batchParse = thrashold
    <$> optional (option auto
        (long "batchEvents" <> help "wait for number of events before send"))
    <*> optional (option kiloMega
        (long "batchBytes" <> help "wait for size in bytes before send"))
    <*> optional (option auto
        (long "batchSeconds" <> help "wait seconds before send"))

dropParse :: Parser Drop
dropParse = subparser $
    command "drop" (info (helper <*> level2) (progDesc "how to drop"))
  where
    level2 = subparser
        ( command "syslog" (info (helper <*> syslog) idm)
       <> command "file" (info (helper <*> file) idm)
        )

    syslog = DSyslog <$> pure SL.Notice <*> formatParse  -- TODO
    --level NOTICE | INFO | ...

    file = DFile
        <$> argument str (metavar "FILE"
            <> help "destination filename, '-' for stdout")
        <*> formatParse

runCmd :: Options -> Action ()
runCmd opts = do
    logM "init" INFO $ show opts
    assert (anyLimit $ optBufferSize opts)
        "Some limit in buffer size is required."

    assert (anyLimit $ optBatchSize opts)
        "Some limit in batch size is required."

    assert (optBatchSize opts `isBelow` optBufferSize opts)
        "Buffer size too small for a given batch size."

    sessionId <- liftIO nextRandom >>= return . U.toString

    recorderId <- do
        -- TODO... get hostname
        return $ fromMaybe "noident" $ optIdent opts

    buf <- liftIO $ atomically $
        newBuffer (optBufferSize opts) (optBatchSize opts)

    input <- startInput
        (optInput opts)
        buf
        (SessionId sessionId)
        (SourceId recorderId)
        (Channel $ optChannel opts)
        (optDrop opts)

    output <- startOutput (optOutput opts) buf

    -- all threads shall remain running
    _ <- liftIO $ waitAnyCatchCancel $ [input, output]
    throw "process terminated"

-- | Copy messages from input to buffer (or call drop handler).
startInput :: Input -> Buffer -> SessionId -> SourceId -> Channel
    -> [Drop] -> Action (Async a)
startInput i buf sessionId recorderId ch dropList = do
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
        notAppended <- atomically $ appendBuffer (snd ts) buf [evt]
        _ <- runAction $ case notAppended of
            [] -> tell DEBUG $ show evt
            [x] -> tell NOTICE $ "error appending: " ++ show x
            _ -> tell ERROR $ "internal error: unexpected events"
        forM_ dropList $ handleDrop notAppended

  where
    createEvent (t1,t2) datagram = Event
        { eChannel = ch
        , eSourceId = recorderId
        , eUtcTime = t1
        , eMonoTime = t2
        , eSessionId = sessionId
        , eValue = datagram
        }

    handleDrop [] _ = return ()
    handleDrop lst dst = case dst of
        DSyslog prio fmt -> SL.withSyslog slOpts $ \syslog -> do
            forM_ lst $ \evt -> do
                let e = encodeEvent fmt evt
                    msg = "["++show i++"] " ++ unpack e
                syslog SL.USER prio $ pack msg
        DFile filename fmt ->
            let s = encodeEvents fmt lst
            in case filename of
                "-" -> BS.hPut System.IO.stdout s
                _ -> BS.appendFile filename s

    slOpts = SL.defaultConfig
        { SL.identifier = pack $ "vcr" -- TODO: use real program name
        }

-- | Copy messages from buffer to output.
startOutput :: Output -> Buffer -> Action (Async ())
startOutput o buf = do
    tell INFO "starting"
    tick <- liftIO (now >>= \(_,t) -> newTVarIO t)
    liftIO $ async $ race_ (ticker tick) (sender tick)

  where
    tell prio msg = logM (show o) prio msg
    tellIO a b = (runAction $ tell a b) >>= \_ -> return ()

    threadDelaySec :: Double -> IO ()
    threadDelaySec = threadDelay . round . (1000000*)

    ticker tick = forever $ do
        threadDelaySec 1
        (_, t) <- now
        atomically $ writeTVar tick t

    sender tick = forever $ do
        evts <- liftIO $ atomically $ readBuffer tick buf
        case o of
            -- send to file
            OFile filename fmt ->
                let s = encodeEvents fmt evts
                in case filename of
                    "-" -> BS.hPut System.IO.stdout s
                    _ -> BS.appendFile filename s

            -- send data over http
            OHttp ip port -> sendHttp ip port evts

    sendHttp ip port evts = do
        -- TODO: add proper content type
        request <- parseRequest $ "PUT http://"++ip++":"++port++"/events"
        let request' = setRequestBodyJSON evts $ request
            retryWith s = do
                tellIO NOTICE s
                threadDelaySec 3
                process
            process = do
                eResponse <- try (httpLBS request')
                case eResponse of
                    Left (SomeException _e) ->
                        retryWith "Unable to connect."
                    Right resp -> do
                        case getResponseStatusCode resp of
                            200 -> tellIO DEBUG "Request processed."
                            _ -> do retryWith $ show resp
        process

