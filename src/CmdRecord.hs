------------------
-- |
-- Module: CmdRecord
--
-- 'record' command
--

module CmdRecord (cmdRecord) where

-- standard imports
import           Control.Exception (onException)
import           Control.Monad
import           Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(INFO, DEBUG, NOTICE))
import           Data.Maybe (fromMaybe)
import           Network.BSD (getHostName)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Pipes
import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as P
import qualified Data.UUID
import           Data.UUID.V4 (nextRandom)
import qualified Control.Concurrent.Async as Async
{-
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Monoid ((<>))
import           System.Environment (getProgName)
import qualified System.Posix.Syslog as SL
-}

-- local imports
import           Common (logM) --, check)
import qualified Common as C
-- import qualified Buffer
import qualified Encodings as Enc
import qualified Event
--import qualified Server
import qualified Udp
import qualified File

{- TODO:
    - check UDP IPv6 unicast/multicast "[ipv6]" instead of "ip"
    - check ipv6 http output
    - ko je connection vzpostavljen,
      naj se buffer prazni postopoma, ne nujno vse naenkrat
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
    , optOutFile    :: Maybe OutputFile
    {-
    , optOnOverflow :: OnOverflow
    , optLimitSend  :: Buffer.Threshold
    , optLimitDrop  :: Buffer.Threshold
    -}
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IStdin Event.Channel
    | IUdp [(Udp.UdpIn, Event.Channel)]
    deriving (Eq, Show)

-- | File output.
data OutputFile = OutputFile Enc.EncodeFormat File.FileStore (Maybe File.Rotate)
    deriving (Eq, Show)

{-
-- | Server output.
data OutputServer = OutputServer Srv.ServerConnection
    deriving (Eq, Show)
-}

{-
-- | Overflow handler (what to do in case of buffer overflow).
data OnOverflow
    = OnOverflowDrop
    | OnOverflowFile File.FileStore
    | OnOverflowSyslog SL.Priority
    deriving (Eq, Show)
-}

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> Opt.optional Event.sourceIdOptions
    <*> (stdinOptions <|> udpInputOptions)
    <*> Opt.optional storeFileOptions
    {-
    <*> storeServerOptions
    <*> (onOverflowDrop <|> onOverflowFile <|> onOverflowSyslog)
    <*> Buffer.thresholdOptions "send"
    <*> Buffer.thresholdOptions "drop"
    -}

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

storeFileOptions :: Opt.Parser OutputFile
storeFileOptions = C.subparserCmd "file ..." $ Opt.command "file" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Store data to a file")
  where
    opts = OutputFile
        <$> Enc.encodeFormatOptions
        <*> File.fileStoreOptions
        <*> Opt.optional File.rotateOptions

{-
storeServerOptions :: Opt.Parser Output
storeServerOptions = OServer <$> srvCmd where
    srvCmd = C.subparserCmd "server ..." $ Opt.command "server" $ Opt.info
        (Srv.serverConnectionOptions <**> Opt.helper)
        (Opt.progDesc "Store data to server(s)")

onOverflowDrop :: Opt.Parser OnOverflow
onOverflowDrop = C.subparserCmd "drop" $ Opt.command "drop" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "In case of buffer overflow, drop data")
  where
    opts = pure OnOverflowDrop

onOverflowFile :: Opt.Parser OnOverflow
onOverflowFile = C.subparserCmd "dropFile ..." $ Opt.command "dropFile" $
    Opt.info (opts <**> Opt.helper)
        (Opt.progDesc "In case of buffer overflow, save data to a file")
  where
    opts = OnOverflowFile <$> File.fileStoreOptions

onOverflowSyslog :: Opt.Parser OnOverflow
onOverflowSyslog = C.subparserCmd "dropSyslog" $ Opt.command "dropSyslog" $
    Opt.info (opts <**> Opt.helper)
        (Opt.progDesc "In case of buffer overflow, send data to syslog")
  where
    opts = OnOverflowSyslog <$> C.syslogOptions
-}

-- | Convert raw data to Event.
toEvents :: Event.Channel -> Event.SourceId
    -> Pipe BS.ByteString Event.Event IO ()
toEvents ch recId = do
    -- Each reader has own session id, so that sequence
    -- numbers can be independant between readers.
    sessionId <- Event.sessionId . Data.UUID.toString <$> lift nextRandom
    let loop seqNum = do
            msg <- await
            (t1,t2) <- lift Event.now
            yield $ Event.Event
                { Event.eChannel = ch
                , Event.eSourceId = recId
                , Event.eUtcTime = t1
                , Event.eMonoTime = t2
                , Event.eSessionId = sessionId
                , Event.eSequence = seqNum
                , Event.eValue = msg
                }
            loop $ Event.nextSequenceNum seqNum
    loop minBound

{-
-- Reader loop is identical for all inputs,
-- except for prepare and actual read functions.
reader :: Show a1 =>
    SL.SyslogConfig
    -> Event.SourceId -> Buffer.Buffer -> OnOverflow
    -> (a1 -> IO (Event.Channel, t))
    -> (t -> IO BS.ByteString)
    -> a1
    -> IO (Async.Async ())
reader syslogOpts recId buf ofAction prepare readData arg = do
    (ch, handle) <- prepare arg

    -- Each reader has own session id, so that sequence
    -- numbers can be independant between readers.
    sessionId <- Event.sessionId . Data.UUID.toString <$> nextRandom
    logM INFO $ "starting input"
        ++ ", ch: " ++ show ch
        ++ ", arg: " ++ show arg
        ++ ", session ID: " ++ show sessionId

    let createEvent (t1,t2) seqNum msg = Event.Event
            { Event.eChannel = ch
            , Event.eSourceId = recId
            , Event.eUtcTime = t1
            , Event.eMonoTime = t2
            , Event.eSessionId = sessionId
            , Event.eSequence = seqNum
            , Event.eValue = msg
            }

        loop seqNum = do
            msg <- readData handle
            ts <- Event.now
            let evt = createEvent ts seqNum msg
            logM DEBUG $ "read event: " ++ show evt

            notAppended <- STM.atomically $
                Buffer.appendBuffer (snd ts) buf [evt]

            case notAppended of
                [] -> return ()
                [x] -> do
                    logM NOTICE $ "overflow: " ++ show x
                    onOverflow ofAction [x]
                _ -> C.throw "unexpected list of not appended events"

            loop $ Event.nextSequenceNum seqNum

    Async.async $ loop minBound
  where

    -- ignore overflow
    onOverflow OnOverflowDrop _ = return ()

    -- append overflow to a file
    onOverflow (OnOverflowFile fs) lst = File.appendFile fs lst

    -- send overflow to syslog
    onOverflow (OnOverflowSyslog level) lst =
        SL.withSyslog syslogOpts $ \syslog -> forM_ lst $ \evt -> do
            let e = Enc.encode (Enc.EncJSON Enc.JSONCompact) evt
                msg = "["++show (Event.eChannel evt)++"] " ++ BSL8.unpack e
            syslog SL.USER level $ BS8.pack msg
-}

{-
-- | Copy messages from buffer to output.
writer :: Output -> Buffer.Buffer -> IO (Async.Async ())
writer out buf = do
    logM INFO $ "starting output: " ++ show out

    -- check rotate options
    _ <- case out of
        OFile _ rot -> runMaybeT $ do
            Rotate keep mSize mTime <- MaybeT $ return rot
            liftIO $ check (keep >= 0)
                "Number of files to keep can not be negative."
            liftIO $ case mSize of
                Nothing -> return ()
                Just size -> check (size >= 0)
                    "Rotate size can not be negative."
            liftIO $ case mTime of
                Nothing -> return ()
                Just time -> check (time >= 0)
                    "Rotate time can not be negative."
        OServer _conn -> return Nothing

    tick <- (snd <$> Event.now) >>= STM.newTVarIO
    Async.async $ Async.race_ (ticker tick) (sender tick)

  where
    threadDelaySec :: Double -> IO ()
    threadDelaySec = threadDelay . round . (1000000*)

    ticker tick = forever $ do
        threadDelaySec 1
        t <- snd <$> Event.now
        STM.atomically $ STM.writeTVar tick t

    sender tick = forever $ do
        evts <- STM.atomically $ Buffer.readBuffer tick buf
        case out of
            OFile fs rot -> do
                -- append data
                File.appendFile fs evts

                -- rotate file
                runMaybeT $ do
                    Rotate keep mSize mTime <- MaybeT $ return rot
                    (new, old) <- MaybeT $ File.rotateFile fs keep mSize mTime
                    liftIO $ do
                        logM INFO $ "Output file rotated: " ++ new
                        forM_ old $ \i -> do
                            logM INFO $ "Old file removed: " ++ i

            OServer _conn -> undefined -- Srv.request??method evts
-}

debugP :: (Show a) => String -> Pipe a a IO ()
debugP prefix = forever $ do
    msg <- await
    lift $ logM DEBUG $ prefix ++ ": " ++ show msg
    yield msg

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "command 'record', opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    {-
    check (Buffer.anyLimit $ optLimitSend opts)
        "Some limit on send size is required."

    check (Buffer.anyLimit $ optLimitDrop opts)
        "Some limit on drop size is required."

    check (optLimitSend opts `Buffer.isBelow` optLimitDrop opts)
        "Buffer drop size too small for a given send size."
    -}

    recId <- do
        hostname <- Event.sourceId <$> getHostName
        return $ fromMaybe hostname $ optIdent opts
    logM INFO $ "recorder ID: " ++ show recId

    {-
    buf <- STM.atomically $
        Buffer.newBuffer (optLimitDrop opts) (optLimitSend opts)

    syslogOpts <- do
        progName <- getProgName
        return $ SL.defaultConfig {SL.identifier = BS8.pack progName}
    -}

    -- prepare application blocks
    let
        -- inputs :: [(String, Producer Event.Event IO ())]
        inputs = case optInput opts of
            IStdin ch -> do
                let msg = show ch
                    pipe = P.stdinLn >-> P.map BS8.pack >-> toEvents ch recId
                return (msg, pipe)
            IUdp src -> do
                (udp,ch) <- src
                let msg = show (udp,ch)
                    pipe = Udp.udpReader udp >-> P.map fst >-> toEvents ch recId
                return (msg, pipe)

        fileOutput :: Consumer Event.Event IO ()
        fileOutput = case optOutFile opts of
            Nothing -> P.drain
            Just (OutputFile fmt fs rotate) ->
                (Enc.toByteString fmt >-> File.fileWriter fs rotate)

    -- merge all inputs to a single stream
    (inputsOut, inputsIn) <- PC.spawn $ PC.bounded $ 2 * (length inputs)

    -- start inputs
    inputsTasks <- forM inputs $ \(msg, i) -> Async.async $
        onException
            (runEffect $ i >-> PC.toOutput inputsOut)
            (logM NOTICE $ "Input error: " ++ msg)

    -- start main process
    mainTask <- Async.async $ do
        -- TODO
        runEffect $ PC.fromInput inputsIn >-> debugP "merged" >-> fileOutput

    -- all threads shall remain running
    _ <- Async.waitAnyCatchCancel (mainTask:inputsTasks)
    C.throw "process terminated"

