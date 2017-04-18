------------------
-- |
-- Module: CmdRecord
--
-- 'record' command
--

module CmdRecord (cmdRecord) where

-- standard imports
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
-}
import qualified Data.UUID
import Data.UUID.V4 (nextRandom)
{-
import Network.HTTP.Simple
import Network.Multicast
import Network.Socket
import Network.Socket.ByteString as NB
import Options.Applicative
import qualified System.IO
-}

-- local imports
import qualified Buffer
import Common (logM, check)
import qualified Common as C
import qualified Event
import qualified Server as Srv
import qualified Udp
import qualified File

{- TODO:
    - check UDP IPv6 unicast/multicast "[ipv6]" instead of "ip"
    - check ipv6 http output
    - ko je connection vzpostavljen,
      naj se buffer prazni postopoma, ne nujno vse naenkrat

    - record naj zna sprejeti listo serverjev,
      če do enega ne more dostopati, gre na drugega...
    -- input: TCPServer Ip Port Format...
    -- input: TCPClient Ip Port Format...
-}

cmdRecord :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdRecord = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event recorder")

-- | Speciffic command options.
data Options = Options
    { optIdent      :: Maybe Event.SourceId
    , optInput      :: Input
    , optOutput     :: Output
    , optOnOverflow :: OnOverflow
    , optLimitSend  :: Buffer.Threshold
    , optLimitDrop  :: Buffer.Threshold
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IStdin Event.Channel
    | IUdp [(Udp.UdpIn, Event.Channel)]
    deriving (Eq, Show)

-- | Output options.
data Output
    = OFile File.FileStore
    | OServer Srv.ServerConnection
    deriving (Eq, Show)

-- | Overflow handler (what to do in case of buffer overflow).
data OnOverflow
    = OnOverflowDrop
    | OnOverflowFile File.FileStore
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> Opt.optional Event.sourceIdOptions
    <*> (stdinOptions <|> udpInputOptions)
    <*> (storeFileOptions <|> storeServerOptions)
    <*> (onOverflowDrop <|> onOverflowFile)
    <*> Buffer.thresholdOptions "send"
    <*> Buffer.thresholdOptions "drop"

stdinOptions :: Opt.Parser Input
stdinOptions = C.subparserCmd "stdin ..." $ Opt.command "stdin" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Read data from stdin")
  where
    opts = IStdin <$> Event.channelOptions

udpInputOptions :: Opt.Parser Input
udpInputOptions = IUdp <$> Opt.some udp where
    udp = C.subparserCmd "udp ..." $ Opt.command "udp" $ Opt.info
        (opts <**> Opt.helper)
        (Opt.progDesc "Read data from UDP")
    opts = (,) <$> Udp.udpInOptions <*> Event.channelOptions

storeFileOptions :: Opt.Parser Output
storeFileOptions = C.subparserCmd "file ..." $ Opt.command "file" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Store data to a file")
  where
    opts = OFile <$> File.fileStoreOptions

storeServerOptions :: Opt.Parser Output
storeServerOptions = OServer <$> srvCmd where
    srvCmd = C.subparserCmd "server ..." $ Opt.command "server" $ Opt.info
        (Srv.serverConnectionOptions <**> Opt.helper)
        (Opt.progDesc "Store data to server(s)")

onOverflowDrop :: Opt.Parser OnOverflow
onOverflowDrop = C.subparserCmd "drop" $ Opt.command "drop" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Drop data on buffer overflow")
  where
    opts = pure OnOverflowDrop

onOverflowFile :: Opt.Parser OnOverflow
onOverflowFile = C.subparserCmd "dropFile ..." $ Opt.command "dropFile" $
    Opt.info (opts <**> Opt.helper)
        (Opt.progDesc "In case of buffer overflow, save data to a file")
  where
    opts = OnOverflowFile <$> File.fileStoreOptions

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "command 'record', opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    check (Buffer.anyLimit $ optLimitSend opts)
        "Some limit on batch size is required."

    check (Buffer.anyLimit $ optLimitDrop opts)
        "Some limit on buffer size is required."

    check (optLimitSend opts `Buffer.isBelow` optLimitDrop opts)
        "Buffer size too small for a given batch size."

    sessionId <- Event.sessionId . Data.UUID.toString <$> nextRandom
    logM INFO $ "session ID: " ++ show sessionId

    print
        ( optIdent opts
        , optInput opts
        , optOutput opts
        , optOnOverflow opts
        )
    {-

    recorderId <- do
        -- TODO... get hostname
        return $ fromMaybe "noident" $ optIdent opts

    buf <- liftIO $ atomically $
        newBuffer (optBufferSize opts) (optLimitSend opts)

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

            -- send data to the server
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
