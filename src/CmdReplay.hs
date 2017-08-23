------------------
-- |
-- Module: CmdReplay
--
-- 'replay' command
--

module CmdReplay (cmdReplay) where

-- Standard imports.
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad
import           Options.Applicative ((<**>)) -- , (<|>))
import qualified Options.Applicative as Opt
import           System.Log.Logger (Priority(INFO, NOTICE))
import           Control.Concurrent (threadDelay)

-- local imports
import           Common (logM)
-- import qualified Buffer
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

-- | Speciffic command options.
data Options = Options
    { --optInput      :: Input
    --, optOutput     :: Output
    --, optBatchSize  :: Buffer.Threshold
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IFile File.FileStore
    | IServer Server.ServerConnection
    deriving (Eq, Show)

-- | Output options.
data Output
    = OStdout Event.Channel
    | OUdp [(Event.Channel, Udp.UdpOut)]
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = pure Options
    {-
    <$> (readFileOptions <|> readServerOptions)
    <*> (stdoutOptions <|> udpOutputOptions)
    -- <*> Buffer.thresholdOptions "batch"
    -}
  where
    {-
    readFileOptions = undefined
    readServerOptions = undefined
    stdoutOptions = undefined
    udpOutputOptions = undefined
    -}

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "replay, opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    runStream $ source >-> cleanup >-> eventFilter >-> timing >-> destination

  where

    -- read from file, decode data
    source =
        File.fileReaderChunks 32752 (File.FileStore "test1.json")
        >-> Encodings.fromByteString (100*1024)
                (Encodings.EncJSON Encodings.JSONCompact)

    -- log errors if any
    cleanup = mkPipe $ \consume produce -> forever $ do
        result <- consume
        case result of
            Left e -> liftIO $ logM NOTICE $ "error: " ++ show e
            Right a -> produce a

    -- pass if channel name has 3 chars or more
    eventFilter = Streams.filter $ \evt ->
        let Event.Channel ch = Event.eChannel evt
        in length ch >= 3

    -- insert some delay between frames
    timing = mkPipe $ \consume produce -> forever $ do
        msg <- consume
        liftIO $ threadDelay . round . (1000000*) $ (0.3 :: Double)
        produce msg

    -- send to stdout
    destination =
        Encodings.toByteString (Encodings.EncJSON Encodings.JSONCompact)
        >-> File.fileWriter (File.FileStore "-") Nothing
        >-> drain

