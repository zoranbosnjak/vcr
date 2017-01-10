{-# LANGUAGE OverloadedStrings #-}

module CmdRecord (cmdRecord) where

import Control.Exception hiding (throw, assert)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forM_)
import Control.Monad.Trans
import Data.ByteString.Char8 (pack)
import Data.Maybe
import qualified Data.UUID as U
import Data.UUID.V4
import Network.HTTP.Simple
import Network.Multicast
import Network.Socket
import Network.Socket.ByteString as NB
import Options.Applicative
import System.Log.Logger (Priority(..))
import qualified System.Posix.Syslog as SL

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
      naj se buffer prazni postopoma, ne nujno vse naenkrat
-}

cmdRecord :: ParserInfo (Action ())
cmdRecord = info (helper <*> (runCmd <$> options))
    (progDesc "event recorder")

data Options = Options
    { optIdent  :: Maybe String
    , optChanel :: String
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
    -- IStdin Format Delimit
    deriving (Show, Eq)

data Output
    = OStdout Format Delimiter
    | OHttp Ip Port
    deriving (Show, Eq)

data Drop
    = DSyslog Format
    | DFile FilePath Format Delimiter
    deriving (Show, Eq)

options :: Parser Options
options = Options
    <$> (optional $ strOption
            (long "ident" <> metavar "IDENT" <> help "recorder identifier"))
    <*> strOption
            (long "chanel" <> metavar "CH" <> help "chanel identifier")
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
        ( command "http" (info (helper <*> http) idm)
       <> command "stdout" (info (helper <*> stdout) idm)
        )
    http = OHttp
        <$> argument str (metavar "IP" <> help "IP address")
        <*> argument str (metavar "PORT" <> help "port number")
    -- TODO: parse stdout options
    --delimit "\n"
    --format JSON | TXT | BSON ... (use automatic choices from??)
    stdout = pure $ OStdout FormatJSON (Delimiter "\n")

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
    --format = json | txt | ...
    --delimit = "\n" | "\0" | "<any string>"
    --level NOTICE | INFO | ...
  where
    level2 = subparser
        ( command "syslog" (info (helper <*> syslog) idm)
       <> command "file" (info (helper <*> file) idm)
        )
    syslog = pure $ DSyslog FormatJSON
    -- TODO, file options
    file = DFile
        <$> argument str (metavar "FILE"
            <> help "destination filename, '-' for stdout")
        <*> pure FormatJSON
        <*> pure (Delimiter "\n")

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
        (Chanel $ optChanel opts)
        (optDrop opts)

    output <- startOutput (optOutput opts) buf

    -- all threads shall remain running
    _ <- liftIO $ waitAnyCatchCancel $ [input, output]
    throw "process terminated"

-- | Copy messages from input to buffer (or call drop handler).
startInput :: Input -> Buffer -> SessionId -> SourceId -> Chanel
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
        { eChanel = ch
        , eSourceId = recorderId
        , eUtcTime = t1
        , eBootTime = t2
        , eSessionId = sessionId
        , eValue = datagram
        }

    handleDrop [] _ = return ()
    handleDrop lst dst = case dst of
        DSyslog _fmt -> SL.withSyslog slOpts $ \syslog -> do
            forM_ lst $ \evt -> do
                --TODO: format message
                let msg = "["++show i++"] " ++ show evt
                syslog SL.USER SL.Notice $ pack msg
        DFile filename _fmt (Delimiter delim) -> do
            -- TODO: format messages
            let msgs = do
                    evt <- lst
                    return $ show evt ++ delim
                s = concat msgs
            case filename of
                "-" -> putStr s
                _ -> appendFile filename s

    slOpts = SL.defaultConfig
        { SL.identifier = pack $ "vcr"
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
        msgs <- liftIO $ atomically $ readBuffer tick buf
        case o of
            -- send data over http
            OHttp ip port -> sendHttp ip port msgs

            -- print to stdout
            -- TODO: use format options
            OStdout _fmt _delimit -> mapM_ print msgs

    sendHttp ip port msgs = do
        request <- parseRequest $ "PUT http://"++ip++":"++port++"/events/json"
        let request' = setRequestBodyJSON msgs $ request
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

