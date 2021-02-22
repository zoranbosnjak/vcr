{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Replay.Engine where

import           GHC.Natural
import           Control.Monad
import           Control.Monad.Fix
import           UnliftIO
import           Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Safe as PS
import           Data.Ratio ((%))
import qualified Data.Map as Map
import qualified Data.Time.Clock as Clk

import           Data.ReactiveValue

-- local imports
import           Common
import           Time
import           Vcr
import           Udp (UdpContent)
import           Replay.Types

bufferSize :: Natural
bufferSize = 10*1000

-- | Channels of interest.
type ChannelSelection = Map.Map Channel
    ( ReactiveFieldRead IO (Maybe ConsoleDump)  -- adjustable
    , Maybe (Consumer (Event UdpContent) (PS.SafeT IO) ())   -- engine restart required
    , Event UdpContent -> IO ()     -- blinker activity trigger
    )

-- | Runtime engine configuration (no engine restart required on change).
data EngineTuning = EngineTuning
    { tunRunningSpeed :: Maybe Double
    , tunMarker1    :: UtcTime
    , tunMarker2    :: UtcTime
    , tunAtMarker   :: AtMarker
    } deriving (Eq, Show)

-- | Engine feedback actions back to user interface.
data EngineFeedback = EngineFeedback
    { fbBufferLevel :: Int -> STM ()    -- updates for buffer level [0..100]
    , fbConsole     :: ConsoleAction -> STM ()  -- dump to console
    }

-- | Static engine configuration (restart engine on change).
data EngineConfig = EngineConfig
    { cfgPlayer     :: Player (PS.SafeT IO) Index (Event UdpContent)
    , cfgDirection  :: Direction
    , cfgChannels   :: ChannelSelection
    , cfgTuning     :: STM EngineTuning
    , cfgFeedback   :: EngineFeedback
    }

-- | Fetch events from player to internal buffer.
fetcher :: Clk.NominalDiffTime -> Direction -> [Channel]
    -> Player (PS.SafeT IO) Index (Event a) -> (Event a -> STM ()) -> UtcTime
    -> IO r
fetcher leakTime direction channels player putBuffer t0 = do
    (ix, event) <- getFirst
    atomically $ putBuffer event
    ixVar <- newTVarIO ix
    fetchFrom ixVar
  where
    -- Get starting (index, event).
    getFirst = fix $ \loop -> do
        result <- tryAny $ PS.runSafeT $ findEventByTimeUtc player t0
        result' <- case result of
            Left _e -> do
                return Nothing
            Right Nothing -> do
                return Nothing
            Right (Just (ix, event)) -> do
                return $ Just (ix, event)
        case result' of
            Just val -> return val
            Nothing -> do
                threadDelaySec 1.0
                loop

    -- Run from starting index and restart in case of problems.
    fetchFrom ixVar = forever $ do
        ix0 <- atomically $ readTVar ixVar
        let flt = onlyChannels channels
            producer = runPlayer player direction ix0
                (Just (flt, Just leakTime))
            consumer = forever $ do
                (ix, event) <- await
                liftIO $ atomically $ do
                    putBuffer event
                    writeTVar ixVar ix

        void $ tryAny $ PS.runSafeT $ runEffect
            -- first element was already processed, so drop 1
            (producer >-> PP.drop 1 >-> consumer)

        threadDelaySec 1.0

-- | Sender process.
-- Use "UTC" time for initial sync, then use monotonic time for actual replay.
sender :: UtcTime
    -> Direction
    -> STM (Event UdpContent)
    -> STM EngineTuning
    -> (UtcTime -> STM ())
    -> (String -> STM ())
    -> ChannelSelection
    -> IO ()
sender t0 direction getFromBuffer getTuning updateT dumpConsole channels = do
    -- input queue and process is required for each channel
    realSenders <- forM channels $ \(consoleRV, mConsumer, blink) -> do
        q <- newTBQueueIO bufferSize
        let producer = forever $ do
                event <- liftIO $ atomically $ readTBQueue q
                yield event
                liftIO $ do
                    blink event
                    reactiveValueRead consoleRV >>= \case
                        Nothing -> return ()
                        Just dump -> atomically $ dumpConsole $ dump event
            consumer = maybe (forever (void await)) id mConsumer
            act = PS.runSafeT $ runEffect $ producer >-> consumer
        return (q, act)

    let senderMap = Map.map fst realSenders

    result <- race (runAll (snd <$> Map.elems realSenders)) $ do
        (evt, mSp) <- atomically ((,) <$> getFromBuffer <*> getRunningSpeed)
        case mSp of
            Nothing -> notRunning senderMap t0 evt
            Just speed -> startup senderMap t0 speed evt
    case result of
        Left n -> fail $ "sender " ++ show n ++ " failed"
        Right val -> return val

  where

    getRunningSpeed :: STM (Maybe Double)
    getRunningSpeed = tunRunningSpeed <$> getTuning

    -- We don't want to drop events when switching between
    -- notRunning and running modes.
    notRunning senderMap tUtc evt = do
        speed <- atomically (getRunningSpeed >>= maybe retrySTM return)
        startup senderMap tUtc speed evt

    -- Calculate how the virtual time will progress,
    -- depending on current time, speed and pending event.
    startup senderMap tUtc speed evt = do
        tStartupMono <- getMonoTimeNs
        let pendingUtc = eTimeUtc evt
            pendingMono = eTimeMono evt
            deltaTNominal = pendingUtc `Clk.diffUTCTime` tUtc
            deltaTNs = round $ (1000*1000*1000)*toRational deltaTNominal
            t0Mono = pendingMono - deltaTNs
            getVirtualMonotonicTime = do
                dtReal <- (\t -> t - tStartupMono) <$> getMonoTimeNs
                let dt = round (fromIntegral dtReal * speed)
                return $ case direction of
                    Forward -> t0Mono + dt
                    Backward -> t0Mono - dt
        running senderMap (eSessionId evt) speed getVirtualMonotonicTime tUtc evt

    -- Timing is only valid within the same session.
    running senderMap session speed getVirtualMonotonicTime = fix $ \loop tUtc evt -> do
        case (eSessionId evt) == session of
            -- new session detected, restart
            False -> startup senderMap tUtc speed evt
            True -> do
                tMono <- getVirtualMonotonicTime
                let (k, compareF) = case direction of
                        Forward -> (1.0, (>=))
                        Backward -> (-1.0, (<=))
                (tUtc', getNextEvent) <- case compareF tMono (eTimeMono evt) of
                    False -> do
                        threadDelaySec 0.001
                        let deltaT = fromRational (round (speed*1000) % (1000*1000))
                            tUtc' = Clk.addUTCTime (deltaT*k) tUtc
                        return (tUtc', pure evt)
                    True -> do
                        fire senderMap evt
                        let tUtc' = eTimeUtc evt
                        return (tUtc', getFromBuffer)
                atomically $ updateT tUtc'
                evt' <- atomically getNextEvent
                mSpeed' <- atomically getRunningSpeed
                case mSpeed' == (Just speed) of
                    False -> case mSpeed' of
                        Nothing -> notRunning senderMap tUtc' evt'
                        Just speed' -> startup senderMap tUtc' speed' evt'
                    True -> do
                        (t1, t2, atMarker) <- atomically getMarkers
                        let limitReached = case direction of
                                Forward  -> tUtc' >= t2
                                Backward -> tUtc' <= t1
                        if
                            -- finish sending, caller will restart the process
                            | limitReached && atMarker == Wrap -> return ()

                            -- wait until something changes, then recalculate
                            | limitReached && atMarker == Stop -> do
                                atomically $ do
                                    result <- getMarkers
                                    when (result == (t1, t2, atMarker)) $ do
                                        retrySTM
                                startup senderMap tUtc' speed evt'

                            -- normal case
                            | otherwise -> loop tUtc' evt'

    -- fire event
    fire senderMap evt = case Map.lookup (eChannel evt) senderMap of
        Nothing -> return ()
        Just q -> atomically $ writeTBQueue q evt

    getMarkers = do
        tun <- getTuning
        return (tunMarker1 tun, tunMarker2 tun, tunAtMarker tun)

-- | Calculate buffer level percentage.
percent :: Clk.NominalDiffTime -> Clk.NominalDiffTime -> Int
percent prefetchSeconds x =
    let a = round (x * 1000)
        b = round (prefetchSeconds * 1000)
        c = (a * 100) `div` b
    in max 0 (min 100 c)

-- | Replay engine process.
engine ::
    Clk.NominalDiffTime
    -> TVar UtcTime
    -> EngineConfig
    -> IO ()
engine prefetchSeconds tUtc cfg = fix $ \loop -> do
    t <- atomically $ readTVar tUtc
    tLastInsertedVar <- newTVarIO t
    buffer <- newTBQueueIO bufferSize

    let fb = cfgFeedback cfg
        getBufferSpan = do
            t1 <- readTVar tLastInsertedVar
            t2 <- readTVar tUtc
            return $ case cfgDirection cfg of
                Forward -> Clk.diffUTCTime t1 t2
                Backward -> Clk.diffUTCTime t2 t1

        putToBuffer event = do
            dt <- getBufferSpan
            when (dt > prefetchSeconds) retrySTM
            writeTBQueue buffer event
            writeTVar tLastInsertedVar $ eTimeUtc event
            getBufferSpan >>= fbBufferLevel fb . percent prefetchSeconds

        getFromBuffer = do
            readTBQueue buffer

        updateT val = do
            writeTVar tUtc val
            getBufferSpan >>= fbBufferLevel fb . percent prefetchSeconds

    result <- race
        (fetcher
            (prefetchSeconds / 10)
            (cfgDirection cfg)
            (Map.keys $ cfgChannels cfg)
            (cfgPlayer cfg) putToBuffer t)
        (sender
            t
            (cfgDirection cfg)
            getFromBuffer
            (cfgTuning cfg)
            updateT
            (fbConsole fb . ConsolePrint)
            (cfgChannels cfg))

    case result of
        Left _ -> fail "fetcher failed"
        Right _ -> do    -- sender terminated
            let tv = case cfgDirection cfg of
                    Forward -> tunMarker1
                    Backward -> tunMarker2
            atomically (fmap tv (cfgTuning cfg) >>= writeTVar tUtc)
            loop

