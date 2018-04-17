------------------
-- |
-- Module: CmdRecord
--
-- 'record' command
--
-- Flow diagram:
--
--        stdIn
--      | [UdpIn] -> (events) -> fork
--                  | toByteString -> WriteBuffer -> Concatinate -> FileOutput
--                  | WriteBuffer -> ServerOutput

{-# LANGUAGE ScopedTypeVariables #-}

--module CmdRecord (cmdRecord) where
module CmdRecord where

-- standard imports
import           Control.Monad.IO.Class (liftIO)
import           Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(INFO, DEBUG))
import qualified Data.ByteString as BS
import qualified Data.UUID
import           Data.UUID.V4 (nextRandom)
{-
--import qualified Data.ByteString.Char8 as BS8
import           System.Environment (getProgName)
import qualified System.Posix.Syslog as SL
-}

-- local imports
import           Common (logM)
import qualified Common as C
import qualified Buffer
import qualified Encodings as Enc
import qualified Event
--import qualified Server
import           Streams
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
    --, optOutServer  :: Maybe OutputServer
    --, optOnOverflow :: OnOverflow
    , optLimitSend  :: Buffer.Threshold
    --, optLimitDrop  :: Buffer.Threshold
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IStdin Event.Channel
    | IUdp [(Udp.UdpIn, Event.Channel)]
    deriving (Eq, Show)

-- | File output.
data OutputFile = OutputFile
    { outFileStore  :: File.FileStore
    , outFileEnc    :: Enc.EncodeFormat
    , outFileRotate :: (Maybe File.Rotate)
    } deriving (Eq, Show)

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
    -- <*> storeServerOptions
    -- <*> (onOverflowDrop <|> onOverflowFile <|> onOverflowSyslog)
    <*> Buffer.thresholdOptions "send"
    -- <*> Buffer.thresholdOptions "drop"

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
        <$> File.fileStoreOptions
        <*> Enc.encodeFormatOptions
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
toEvents :: Event.Channel -> Event.SourceId -> Pipe BS.ByteString Event.Event
toEvents ch recId = mkPipe $ \consume produce -> do
    -- Each reader has own session id, so that sequence
    -- numbers can be independant between readers.
    sessionId <- liftIO $ Event.sessionId . Data.UUID.toString <$> nextRandom
    let loop seqNum = do
            msg <- consume Clear
            (t1,t2) <- liftIO $ Event.now
            produce $ Event.Event
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

-- | Log every messages.
debugP :: (Show a) => String -> Pipe a a
debugP prefix = mkPipe $ \consume produce -> forever $ do
    msg <- consume Clear
    liftIO $ logM DEBUG $ prefix ++ ": " ++ show msg
    produce msg

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "command 'record', opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    {-
    check (Buffer.thresholdValid $ optLimitSend opts)
        "Some limit on send size is required."

    {-
    check (Buffer.thresholdValid $ optLimitDrop opts)
        "Some limit on drop size is required."

    check (optLimitSend opts `Buffer.isBelow` optLimitDrop opts)
        "Buffer drop size too small for a given send size."
    -}

    recId <- do
        hostname <- Event.sourceId <$> getHostName
        return $ fromMaybe hostname $ optIdent opts
    logM INFO $ "recorder ID: " ++ show recId

    {-
    syslogOpts <- do
        progName <- getProgName
        return $ SL.defaultConfig {SL.identifier = BS8.pack progName}
    -}

    -- prepare streaming components
    let
        inputs :: [Producer Event.Event]
        inputs = case optInput opts of
            IStdin ch -> return $
                File.fileReaderLines (File.FileStore "-") >-> toEvents ch recId
            IUdp src -> do
                (udp,ch) <- src
                return $ onTerminate
                    (liftIO $ logM NOTICE $ "Input error: " ++ show (udp,ch))
                    (Udp.udpReader udp
                        >-> Streams.map fst
                        >-> toEvents ch recId
                    )
        fileOutput :: Consumer Event.Event
        fileOutput = case optOutFile opts of
            Nothing -> drain
            Just outFile ->
                Enc.toByteString (outFileEnc outFile)
                >-> Buffer.holdBuffer (optLimitSend opts)
                >-> Buffer.concatinated
                >-> onTerminate
                    (liftIO $ logM NOTICE $ "Output error: " ++ show outFile)
                    (File.fileWriter
                        (outFileStore outFile)
                        (outFileRotate outFile))
                >-> dumpRotateMessages

        dumpRotateMessages = mkConsumer $ \consume -> forever $ do
            msg <- consume Clear
            liftIO $ logM INFO msg

        webOutput :: Consumer Event.Event
        webOutput = debugP "web" >-> drain -- TODO

    -- run the chain
    runStream $
        mergeStreams inputs
        >-> debugP "events"
        >-> forkStreams [fileOutput, webOutput]
    -}

