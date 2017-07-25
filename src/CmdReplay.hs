------------------
-- |
-- Module: CmdReplay
--
-- 'replay' command
--

module CmdReplay (cmdReplay) where

cmdReplay = undefined

{-
-- standard imports
import Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import System.Log.Logger (Priority(INFO))

-- local imports
import qualified Buffer
import Common (logM)
import qualified Common as C
import qualified Event
import qualified Server as Srv
import qualified File
import qualified Udp

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
    { optInput      :: Input
    , optOutput     :: Output
    , optBatchSize  :: Buffer.Threshold
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IFile File.FileStore
    | IServer Srv.ServerConnection
    deriving (Eq, Show)

-- | Output options.
data Output
    = OStdout Event.Channel
    | OUdp [(Event.Channel, Udp.UdpOut)]
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> (readFileOptions <|> readServerOptions)
    <*> (stdoutOptions <|> udpOutputOptions)
    <*> Buffer.thresholdOptions "batch"
  where
    readFileOptions = undefined
    readServerOptions = undefined
    stdoutOptions = undefined
    udpOutputOptions = undefined

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "replay, opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    print
        ( optInput opts
        , optOutput opts
        , optBatchSize opts
        )
-}

