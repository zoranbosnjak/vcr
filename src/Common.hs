------------------
-- |
-- Module: Common
--
-- This module provides common VCR definitions.
--

{-# LANGUAGE LambdaCase #-}

module Common
( now, nowMono, nowUtc
, monoTimeToSeconds
, logM
, threadDelaySec
, Log.Priority(..)
, VcrOptions(..)
, vcrOptions
, timeOptions
, subparserCmd
, throw
, kiloMega
, drain
, feed
, forkConsumers
, consumeWith
, linkAsync
) where

import           Data.Bool
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception (throwIO, Exception)
import           Control.Monad.Fix
import           Control.DeepSeq
import           Data.Monoid ((<>))
import           Data.Time (UTCTime)
import           Data.Typeable (Typeable)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Builder.Internal as Opt.Int
import           Options.Applicative.Types (OptReader(CmdReader))
import qualified System.Log.Logger as Log
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Text.Read (readMaybe)
import qualified System.Clock
import qualified Data.Time

import           Pipes
import qualified Pipes.Safe as PS
import           Pipes.Internal (Proxy(Request, Respond, M, Pure), closed)
import qualified Pipes.Concurrent as PC

-- | Get current time (monotonic, UTC).
now :: IO (System.Clock.TimeSpec, UTCTime)
now = (,)
    <$> System.Clock.getTime System.Clock.Boottime
    <*> Data.Time.getCurrentTime

nowMono :: IO System.Clock.TimeSpec
nowMono = fst <$> now

nowUtc :: IO UTCTime
nowUtc = snd <$> now

-- | Convert monotonic time to seconds.
monoTimeToSeconds :: Fractional a => System.Clock.TimeSpec -> a
monoTimeToSeconds t =
    (/ (10^(9::Int))) $ fromInteger $ System.Clock.toNanoSecs t

------------------------------------------------------------------------------
-- Logging and error reporting.

newtype VcrError = VcrError String deriving (Eq, Typeable)
instance Exception VcrError
instance Show VcrError where
    show (VcrError err) = err

threadDelaySec :: Double -> IO ()
threadDelaySec = threadDelay . round . (1000000*)

-- | Throw VCR exception.
throw :: String -> IO a
throw s = do
    let err = VcrError s
    logM Log.ERROR $ show err
    throwIO err

-- | Log a message.
logM :: Log.Priority -> String -> IO ()
logM = Log.logM "vcr"

------------------------------------------------------------------------------
-- Support for commmand line option parsing.

-- | General VCR command line options.
data VcrOptions = VcrOptions
    { vcrOptVerbose :: Maybe Log.Priority
    , vcrOptSyslog  :: Maybe Log.Priority
    , vcrOptEkg     :: Maybe (BS.ByteString, Int)
    } deriving (Show)

-- | A parser for parsing general VCR command line options.
vcrOptions :: Opt.Parser VcrOptions
vcrOptions = VcrOptions
    <$> Opt.optional (Opt.option Opt.auto
        ( Opt.short 'v'
       <> Opt.long "verbose"
       <> Opt.metavar "LEVEL"
       <> Opt.help ("Set console verbosity level, one of: " ++ show levels)
        ))
    <*> Opt.optional (Opt.option Opt.auto
        ( Opt.long "syslog"
       <> Opt.metavar "LEVEL"
       <> Opt.help ("Set syslog verbosity level, one of: " ++ show levels)
        ))
    <*> Opt.optional (Opt.option ekgOption
        ( Opt.long "ekg"
       <> Opt.metavar "IP:PORT"
       <> Opt.help "Enable EKG monitor"
        ))
  where
    levels = [minBound..maxBound] :: [Log.Priority]
    ekgOption = Opt.maybeReader $ \s -> do
        let (a,b) = break (==':') s
            ip = BS8.pack a
        port <- readMaybe $ drop 1 b
        Just (ip, port)

-- | Options for start/stop time.
timeOptions :: String -> Opt.Parser UTCTime
timeOptions _s = undefined

-- | This is a copy of 'subparser' function with the addition
-- of one more argument instead of fixed "COMMAND" string.
subparserCmd :: String -> Opt.Int.Mod Opt.Int.CommandFields a -> Opt.Parser a
subparserCmd disp m = Opt.Int.mkParser d g rdr
  where
    Opt.Int.Mod _ d g = Opt.metavar disp `mappend` m
    (groupName, cmds, subs) = Opt.Int.mkCommand m
    rdr = CmdReader groupName cmds subs

-- | Helper function to convert eg. 1k -> 1024
kiloMega :: Opt.ReadM Integer
kiloMega = Opt.eitherReader $ \arg -> do
    let (a,b) = case last arg of
            'k' -> (init arg, 1)
            'M' -> (init arg, 2)
            'G' -> (init arg, 3)
            _ -> (arg, 0)
    case reads a of
        [(r, "")] -> return (r * (1024^(b::Int)))
        _         -> Left $ "cannot parse value `" ++ arg ++ "'"

-- | Custom version of 'drain', where each element is
-- fully evaluated, to prevent a leak.
drain :: (NFData a, Monad m) => Consumer a m r
drain = await >>= \x -> x `deepseq` drain

-- | Check if the consumer is ready to accept new data.
feed :: Monad m => Consumer a m r -> m (Either r (a -> Consumer a m r))
feed = go
  where
    go p = case p of
        Request _ fu -> return (Right fu)
        Respond v _  -> closed v
        M         m  -> m >>= go
        Pure    r    -> return (Left r)

-- | Distribute messages to multiple consumers.
forkConsumers :: (NFData a, Monad m) => [Consumer a m r] -> Consumer a m r
forkConsumers [] = drain
forkConsumers cs = go cs
  where
    go lst = do
        ready <- sequence <$> mapM (lift . feed) lst
        case ready of
            Left r -> return r
            Right fs -> do
                msg <- await
                go (map ($ msg) fs)

-- | Restart consumer on every reconfiguration.
-- Make sure not to loose events even during reconfiguration.
consumeWith :: (Eq t, Show t, PS.MonadSafe m) => String
    -> STM t
    -> (t -> Consumer a (PS.SafeT IO) r)
    -> Consumer a m r
consumeWith name getVar mkConsumer = do
    q <- liftIO $ newEmptyTMVarIO
    val <- liftIO $ atomically getVar
    go q val
  where
    go q val = do
        liftIO $ logM Log.INFO $ "restarting " ++ show name ++ ": " ++ show val
        rv <- PS.bracket prepare finalize action
        either return (go q) rv
      where

        newVal = getVar >>= \x -> bool retry (return x) (x /= val)

        waitEmpty = isEmptyTMVar q >>= bool retry (return ())

        p = fix $ \loop -> do
            rv <- liftIO $ atomically $
                fmap Left newVal `orElse` fmap Right (takeTMVar q)
            case rv of
                Left x -> return x
                Right msg -> do
                    yield msg
                    loop

        prepare = liftIO $ Async.async $ do
            rv <- PS.runSafeT $ runEffect $
                fmap Right p >-> fmap Left (mkConsumer val)
            PC.performGC
            return rv

        finalize a = liftIO $ Async.cancel a

        action a = fix $ \loop -> do
            rv <- liftIO $ atomically $ do
                fmap Left (Async.waitSTM a) `orElse` fmap Right waitEmpty
            case rv of
                Left r -> return r
                Right () -> do
                    msg <- await
                    liftIO $ atomically $ putTMVar q msg
                    loop

-- | Start async process and link to the current thread.
linkAsync :: IO a -> IO (Async.Async a)
linkAsync act = do
    a <- Async.async act
    Async.link a
    return a

