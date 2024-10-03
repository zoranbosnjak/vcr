{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- | VCR 'cat' command.

module CmdCat where

-- standard imports
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set              as Set
import qualified Data.Text.IO          as T
import           GHC.Generics          (Generic)
import           Options.Applicative
import           Pipes
import qualified Pipes.Safe            as PS
import           UnliftIO

-- local imports
import           Common
import           Streaming
import           Time
import           Udp
import           Vcr

data DumpMode
    = Replay Direction
    | Follow
    deriving (Generic, Eq, Show)

data Subcommand
    = ShowLimits
    | ShowChannels
    | DumpData DumpMode [Channel]
    deriving (Generic, Eq, Show)

data CmdOptions = CmdOptions
    { optSource :: Source
    , optT1     :: Maybe UtcTime
    , optT2     :: Maybe UtcTime
    , optCmd    :: Subcommand
    } deriving (Generic, Eq, Show)

options :: Parser CmdOptions
options = CmdOptions
    <$> (srcFile <|> srcDir <|> srcHttp)
    <*> optional (option (eitherReader parseIsoTime) (long "start" <> help "start time"))
    <*> optional (option (eitherReader parseIsoTime) (long "stop" <> help "stop time"))
    <*> (showLimits <|> showChannels <|> dumpData)
  where
    srcFile = SFile
        <$> pure TextEncoding
        <*> strOption (long "file" <> metavar "FILE" <> help "file backend")

    srcDir = SDirectory
        <$> pure TextEncoding
        <*> strOption (long "dir" <> metavar "FILE"
                       <> help "directory backend (base filename)")

    srcHttp = SHttp
        <$> strOption (long "url" <> metavar "URL" <> help "url backend")

    showLimits :: Parser Subcommand
    showLimits = flag' ShowLimits (long "limits" <> help "Show limits and exit")

    showChannels :: Parser Subcommand
    showChannels = flag' ShowChannels (long "channels" <> help "Show channels and exit")

    dumpData :: Parser Subcommand
    dumpData = DumpData
        <$> (follow <|> fmap Replay replay)
        <*> many (strOption (long "channel" <> metavar "CH" <> help "channel identifier"))
      where
        follow = flag' Follow (long "follow" <> short 'f' <> help "Wait until new data is available")
        replay = flag Forward Backward (long "backward" <> help "Run backward in time")

runCmd :: CmdOptions -> Prog -> Args -> Version -> IO ()
runCmd opt _pName _pArgs _version = do
    let player :: Player (PS.SafeT IO) Index (Event UdpContent)
        player = mkPlayer $ optSource opt

        t1 = optT1 opt
        t2 = optT2 opt

        findTime t = PS.runSafeT (findEventByTimeUtc player t) >>= \case
            Nothing -> fail $ "Can not find time: " <> show t
            Just (ix, _event) -> pure ix

        timeCheck Forward  _mt1 Nothing _t    = True
        timeCheck Forward  _mt1 (Just tLim) t = t <= tLim
        timeCheck Backward Nothing _mt2 _t    = True
        timeCheck Backward (Just tLim) _mt2 t = t >= tLim

    case optCmd opt of

        ShowLimits -> do
            (limit1, limit2) <- PS.runSafeT $ limits player
            PS.runSafeT (peekItem player limit1) >>= (putStrLn . fmtTime) . eTimeUtc
            PS.runSafeT (peekItem player limit2) >>= (putStrLn . fmtTime) . eTimeUtc

        ShowChannels -> do
            (limit1, _limit2) <- PS.runSafeT $ limits player
            startIx <- maybe (pure limit1) findTime (optT1 opt)

            let producer :: Producer (Index, Event UdpContent) (PS.SafeT IO) ()
                producer = runPlayer player Forward startIx Nothing

                consumer :: Set.Set Channel -> Consumer (Index, Event UdpContent) (PS.SafeT IO) ()
                consumer acc = do
                    (_ix, event) <- await
                    let ch = eChannel event
                    when (timeCheck Forward t1 t2 $ eTimeUtc event) $ do
                        if
                            | Set.member ch acc -> consumer acc
                            | otherwise -> do
                                liftIO $ T.putStrLn ch
                                consumer (Set.insert ch acc)

            PS.runSafeT $ runEffect (producer >-> consumer mempty)

        DumpData mode channels -> do
            let flt = case channels of
                    []  -> Nothing
                    lst -> Just (onlyChannels lst, Nothing)

            case mode of
                Replay direction -> do
                    (limit1, limit2) <- PS.runSafeT $ limits player
                    startIx <- case direction of
                        Forward  -> maybe (pure limit1) findTime (optT1 opt)
                        Backward -> maybe (pure limit2) findTime (optT2 opt)

                    let producer :: Producer (Index, Event UdpContent) (PS.SafeT IO) ()
                        producer = runPlayer player direction startIx flt

                        consumer :: Consumer (Index, Event UdpContent) (PS.SafeT IO) ()
                        consumer = do
                            (_ix, event) <- await
                            when (timeCheck direction t1 t2 $ eTimeUtc event) $ do
                                liftIO $ BS8.putStrLn $ encodeJSON event
                                consumer

                    PS.runSafeT $ runEffect (producer >-> consumer)

                Follow -> do
                    let dt = 0.3
                        go ix = do
                            result <- tryAny $ PS.runSafeT $ nextItem player Forward ix flt
                            case result of
                                Left _e -> do
                                    threadDelaySec dt
                                    go ix
                                Right (ix', event) -> do
                                    liftIO $ BS8.putStrLn $ encodeJSON event
                                    go ix'

                    (_limit1, limit2) <- fix $ \loop -> do
                        result <- tryAny $ PS.runSafeT $ limits player
                        case result of
                            Left _e -> do
                                threadDelaySec dt
                                loop
                            Right val -> pure val

                    go limit2

-- | toplevel command
cmdCat :: ParserInfo Command
cmdCat = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Dump events")
