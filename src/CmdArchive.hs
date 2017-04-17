------------------
-- |
-- Module: CmdArchive
--
-- 'archive' command
--

module CmdArchive (cmdArchive) where

-- standard imports
--import Data.Time (UTCTime(UTCTime))
import Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opt
import System.Log.Logger (Priority(INFO))

-- local imports
--import qualified Buffer
import Common (logM)
import qualified Common as C
import qualified Event
import qualified Server as Srv
import qualified File

cmdArchive :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdArchive = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event archiver")

-- | Speciffic command options.
data Options = Options
    { optInput          :: Input
    , optOutput         :: Output
    , optChannelFilter  :: [Event.Channel]
    , optSourceIdFilter :: [Event.SourceId]
    -- TODO: , optStartTime      :: Maybe UTCTime
    -- TODO: , optStopTime       :: Maybe UTCTime
    -- TODO: , read/write batch size
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IFile File.FileStore
    | IServer Srv.Server
    deriving (Eq, Show)

-- | Output options.
data Output
    = OFile File.FileStore
    | OServer Srv.Server
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> input
    <*> output
    <*> Opt.many Event.channelOptions
    <*> Opt.many Event.sourceIdOptions
    -- <*> Opt.optional (C.timeOptions "start")
    -- <*> Opt.optional (C.timeOptions "stop")

input :: Opt.Parser Input
input = C.subparserCmd "input ..." $ Opt.command "input" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data source")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *> (IFile <$> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server") *> (IServer <$> Srv.serverOptions)

output :: Opt.Parser Output
output = C.subparserCmd "output ..." $ Opt.command "output" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data destination")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *> (OFile <$> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server") *> (OServer <$> Srv.serverOptions)

-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    logM INFO $
        "archive, opts: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    putStrLn "GO ARCHIVE!"
    print
        ( optInput opts
        , optOutput opts
        , optChannelFilter opts
        , optSourceIdFilter opts
        )

