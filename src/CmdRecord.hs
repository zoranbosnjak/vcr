------------------
-- |
-- Module: CmdRecord
--
-- 'record' command implementation
--

-- {-# LANGUAGE OverloadedStrings #-}

module CmdRecord (cmdRecord) where

-- standard imports
import Data.Monoid ((<>))
import Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import System.Log.Logger (Priority(INFO))

{-
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
import qualified System.IO
import qualified System.Posix.Syslog as SL
-}

-- local imports
import Common (logM)
import qualified Common as C
import qualified Encodings as Enc
{-
import Buffer
import Event
-}

{- TODO:
    - check UDP IPv6 unicast/multicast "[ipv6]" instead of "ip"
    - check ipv6 http output
    - ko je connection vzpostavljen,
      naj se buffer prazni postopoma, ne nujno vse naenkrat

    - record naj zna snemati več kanalov na enkrat

    - record naj zna sprejeti listo serverjev,
      če do enega ne more dostopati, gre na drugega...
    -- input: IStdin Format...
    -- input: TCPServer Ip Port Format...
    -- input: TCPClient Ip Port Format...
-}

{-
cmdRecord :: ParserInfo (Action ())
cmdRecord = info (helper <*> (runCmd <$> options))
    (progDesc "event recorder")
-}

cmdRecord :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdRecord = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event recorder.")

-- | Speciffic command options.
data Options = Options
    { optIdent :: Maybe String
    , optChannel :: String
    , optInput :: Input
    --, optOutput :: Output
    {-
    , optBufferSize :: Thrashold
    , optBatchSize :: Thrashold
    , optDrop :: [Drop]
    -- retry http connect interval
    -- http response timeout
    -}
    } deriving (Show, Eq)

-- | Command option parser
options :: Opt.Parser Options
options = Options
    <$> (Opt.optional $ Opt.strOption
            ( Opt.long "ident"
           <> Opt.metavar "IDENT"
           <> Opt.help "Recorder identifier"
            )
        )
    <*> Opt.strOption
            ( Opt.short 'c'
           <> Opt.long "channel"
           <> Opt.metavar "CH"
           <> Opt.help "Channel identifier"
            )
    <*> (stdInput <|> udpInput)

    -- <*> parseOutput

-- | Input options.
data Input
    = IStdin
    | IUdp [C.Udp]
    deriving (Show, Eq)

stdInput :: Opt.Parser Input
stdInput = Opt.flag' IStdin (Opt.long "stdin" <> Opt.help "Read from stdin")

udpInput :: Opt.Parser Input
udpInput = IUdp <$> Opt.some (C.udpAs 'i' "input")

{-
udpInput :: Opt.Parser Input
udpInput = IUdp <$> Opt.some (Opt.option (Opt.maybeReader readMaybe)
        ( Opt.short 'i'
       <> Opt.long "input"
       <> Opt.metavar "ARG"
       <> Opt.help (C.showOpt C.udpOptions)
        )
    )
-}

{-
-- | Output options: to a file or to set of http servers
data Output
    = OFile C.FileStore
    | OHttp [C.URI]
    deriving (Show, Eq)

-- | Parse output options.
parseOutput :: Opt.Parser Output
parseOutput =
    ( OFile <$> Opt.option Opt.auto
        (Opt.long "file"
       <> Opt.help "TODO..."
        )
    )
   <|>
    ( OHttp <$> Opt.strOption
        ( Opt.long "http"
       <> Opt.metavar "URI"
       <> Opt.help "Address of the server"
        )
    )
-}

{-
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
-}

-- | Run actual command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "command 'record', opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

{-
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

    let loop seqNum = do
            (datagram, _addr) <- NB.recvFrom sock (2^(16::Int))
            ts <- now
            let evt = createEvent ts seqNum datagram
            notAppended <- atomically $ appendBuffer (snd ts) buf [evt]
            _ <- runAction $ case notAppended of
                [] -> tell DEBUG $ show evt
                [x] -> tell NOTICE $ "error appending: " ++ show x
                _ -> tell ERROR $ "internal error: unexpected events"
            forM_ dropList $ handleDrop notAppended
            loop $ nextSequenceNum seqNum

    liftIO $ async $ loop minBound

  where
    createEvent (t1,t2) seqNum datagram = Event
        { eChannel = ch
        , eSourceId = recorderId
        , eUtcTime = t1
        , eMonoTime = t2
        , eSessionId = sessionId
        , eSequence = seqNum
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
-}
