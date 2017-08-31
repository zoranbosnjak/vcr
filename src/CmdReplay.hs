------------------
-- |
-- Module: CmdReplay
--
-- 'replay' command
--

module CmdReplay (cmdReplay) where

-- Standard imports.
import           Control.Monad.IO.Class (liftIO)
import           Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(INFO, NOTICE))
import           Control.Concurrent (threadDelay)
import           Data.Monoid

-- local imports
import           Common (logM)
import qualified Common as C
import qualified Event
import qualified Server
import qualified File
import qualified Encodings
import qualified Udp
import           Streams

{-
    - GUI or txt
    - is able to use dump file or http (server)
    - channel selection for replay
    - replay sessions
    - possibility to attach filters on replay (eg. restamp inside asterix)
    - check IPv6
-}

cmdReplay :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdReplay= Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event replay")

-- | Channel timeout in seconds. If there is no data within a period,
-- assume the channel is idle.
type Timeout = Double

-- | Speciffic command options.
data Options = Options
    { optInput      :: Input
    , optOutput     :: [Output]
    -- optSession       (only use speciffic session ID -
    --                  do not switch between different recording sessions)
    -- optFailOnSeqError :: Bool (if sequence error is found, quit)
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IFile Encodings.EncodeFormat File.FileStore
    | IServer Server.ServerConnection
    deriving (Eq, Show)

-- | Output options.
data Output = Output Event.Channel Destination Timeout
    deriving (Eq, Show)

data Destination
    = DstStdout
    | DstUdp Udp.UdpOut
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> input
    <*> Opt.some output

input :: Opt.Parser Input
input = C.subparserCmd "input ..." $ Opt.command "input" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data source")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *>
        (IFile <$> Encodings.encodeFormatOptions <*> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server")
        *> (IServer <$> Server.serverConnectionOptions)

output :: Opt.Parser Output
output = C.subparserCmd "output ..." $ Opt.command "output" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Destination")
  where
    opts = Output <$> ch <*> dst <*> timeout
    ch = Event.Channel <$> Opt.strOption
        ( Opt.long "channel"
       <> Opt.metavar "CHANNEL"
       <> Opt.help "channel name"
        )
    dst = dstStdout <|> dstUdp
    dstStdout = Opt.flag' DstStdout (Opt.long "stdout")
    dstUdp = Opt.flag' () (Opt.long "udp" )
        *> (DstUdp <$> Udp.udpOutOptions)
    timeout = Opt.option Opt.auto
        ( Opt.long "timeout"
       <> Opt.metavar "TIMEOUT"
       <> Opt.help "stream timeout in seconds"
        )

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "replay, opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    runStream $ source >-> cleanup >-> forkStreams (toOutput <$> optOutput opts)

  where

    -- read from file, decode data
    source = case optInput opts of
        IFile enc fs ->
            File.fileReaderChunks 32752 fs
            >-> Encodings.fromByteString (100*1024) enc
        IServer _sc -> undefined

    -- log errors if any
    cleanup = mkPipe $ \consume produce -> forever $ do
        result <- consume
        case result of
            Left e -> liftIO $ logM NOTICE $ "error: " ++ show e
            Right a -> produce a

    -- individual output handler
    toOutput (Output ch dst timeout) =
        Streams.filter (\evt -> Event.eChannel evt == ch)
        >-> timing timeout
        >-> Streams.map Event.eValue
        >-> destination dst

    -- provide send timing
    timing _timeout = mkPipe $ \consume produce -> do
        let nowMono = Event.monoTimeToSeconds . snd <$> Event.now
            eMono = Event.monoTimeToSeconds . Event.eMonoTime
            threadDelaySec = threadDelay . round . (1000000*)
        -- consume first event and current monotonic time
        -- this monotonic time is kind of "zero" time
        firstEvt <- consume
        zeroTime <- liftIO nowMono
        let offset = zeroTime - eMono firstEvt

        let loop evt = do
                t <- liftIO nowMono
                let fireAt = eMono evt + offset
                    waitTime = fireAt - t
                liftIO $ threadDelaySec $ max (0::Double) waitTime
                produce evt
                consume >>= loop

        loop firstEvt

    -- send to destination
    destination dst = case dst of
        DstStdout -> mkConsumer $ \consume -> forever $ do
            s <- consume
            liftIO $ putStrLn $ Encodings.hexlify s
        DstUdp udp -> onTerminate
            (liftIO $ logM NOTICE $ "Output error: " ++ show udp)
            (Udp.udpWriter udp)

