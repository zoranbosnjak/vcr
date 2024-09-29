{-# LANGUAGE LambdaCase #-}

-- | This module implements VCR controller.
-- The controller creates and connects required reactive values,
-- then starts the main VCR 'engine' process.

module Replay.Controller where

import           Control.Monad
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.ReactiveValue.Extended
import qualified Pipes.Safe                  as PS
import           UnliftIO

-- local imports
import           Common
import           Replay.Engine
import           Replay.Types
import           Streaming
import           Time
import           Udp                         (UdpContent)
import           Vcr

-- | Check task and exit if not running.
checkTask :: UI -> String -> Async () -> IO ()
checkTask ui name task = poll task >>= \case
    Nothing -> pure ()
    Just x -> do
        print $ name ++ " terminated, result: " ++ show (x :: Either PS.SomeException ())
        uiExit ui

-- | Limit monitor for selected player.
limitMonitor ::
    (Maybe (UtcTime, UtcTime) -> STM ())    -- update limits action
    -> Player (PS.SafeT IO) ix (Event a)    -- selected player
    -> IO r
limitMonitor updateLimits player = do
    atomically $ updateLimits Nothing   -- initial update on process reset
    periodic 1.0 $ do
        result <- tryAny $ do
            (ix1, ix2) <- PS.runSafeT $ limits player
            a <- PS.runSafeT $ peekItem player ix1
            b <- PS.runSafeT $ peekItem player ix2
            pure (eTimeUtc a, eTimeUtc b)
        atomically $ updateLimits $ case result of
            Left _    -> Nothing
            Right val -> Just val

-- | Determine channel selection from:
--  - selected output set
--  - selected console outputs
--  - selected udp outputs
-- Resulting reactive value will only propagate changes when necessary.
-- That is:
--  - if total channel set changes
--  - if any udp output is selected/unselected
channelSelectionR :: UI -> IO (ReactiveFieldRead IO ChannelSelection)
channelSelectionR ui = do
    notifiers <- newMVar []
    let getter = do
            i <- reactiveValueRead (uiOutputSet ui)
            let s = fromJust $ lookup i (uiOutputSelection ui)
            result <- forM s $ \(channel, (consoleRV, senderRV, blink)) -> do
                mConsole <- reactiveValueRead consoleRV
                mSender <- reactiveValueRead senderRV
                let required = isJust mConsole || isJust mSender
                pure $ case required of
                    False -> Nothing
                    True -> Just (channel, (readOnly consoleRV, mSender, blink))
            pure $ Map.fromList $ catMaybes result

        notifier p = modifyMVar_ notifiers (\x -> pure (x ++ [p]))

        getChannelSelection :: IO (Name, Map.Map Channel (Bool, Bool))
        getChannelSelection = do
            i <- reactiveValueRead (uiOutputSet ui)
            let f (consoleRV, mSender, _blink) = do
                    mConsole <- reactiveValueRead consoleRV
                    pure (isJust mConsole, isJust mSender)
            result <- getter >>= traverse f
            pure (i, result)

    oldValueVar <- getChannelSelection >>= newMVar
    let update = do
            newValue <- getChannelSelection
            oldValue <- swapMVar oldValueVar newValue
            let name1 = fst oldValue
                name2 = fst newValue
                required1 = Map.map (uncurry (||)) (snd oldValue)
                required2 = Map.map (uncurry (||)) (snd newValue)
                output1 = Map.map snd (snd oldValue)
                output2 = Map.map snd (snd newValue)
            let restartFlag
                    = name2 /= name1
                    || required2 /= required1
                    || output2 /= output1
            when restartFlag (readMVar notifiers >>= sequence_)

    reactiveValueOnCanRead (uiOutputSet ui) update
    forM_ (uiOutputSelection ui) $ \(_name, lst) -> do
        forM_ lst $ \(_channel, (consoleRV, senderRV, _blink)) -> do
            reactiveValueOnCanRead consoleRV update
            reactiveValueOnCanRead senderRV update
    pure $ ReactiveFieldRead getter notifier

-- | Create and connect reactive values, start monitor and engine processes.
controller :: TVar UtcTime -> UI -> IO (ReactiveFieldRead IO EngineConfig, IO ())
controller tUtc ui = do
    let selectedPlayerRV :: ReactiveFieldRead IO (Player (PS.SafeT IO) Index (Event UdpContent))
        selectedPlayerRV = liftR3 f (uiSource ui) (uiRecorder ui) (uiFile ui) where
            f UISourceRecorder src _file = mkPlayer src
            f UISourceFile _ Nothing = mkDummyPlayer
            f UISourceFile _ (Just filename) = mkPlayer (SFile TextEncoding filename)

        speedRV :: ReactiveFieldRead IO (Maybe Double)
        speedRV = liftR2 f (uiRunning ui) (uiSpeed ui) where
            f False _  = Nothing
            f True val = Just val

        engineTuningRV :: ReactiveFieldRead IO EngineTuning
        engineTuningRV = liftR4 EngineTuning
            speedRV
            (uiMarker1 ui)
            (uiMarker2 ui)
            (uiAtMarker ui)

    engineTuningSTM <- do
        var <- reactiveValueRead engineTuningRV >>= newTVarIO
        follow engineTuningRV $ atomically . writeTVar var
        pure var

    let engineFeedback :: EngineFeedback
        engineFeedback = EngineFeedback
            (uiBufferLevel ui)
            (uiConsole ui)

    channelSelectionRV <- channelSelectionR ui

    let realCSelRV :: ReactiveFieldRead IO ChannelSelection
        realCSelRV = liftR2 Map.mapKeys (uiChannelMap ui) channelSelectionRV

        engineConfigRV :: ReactiveFieldRead IO EngineConfig
        engineConfigRV = liftR5 EngineConfig
            selectedPlayerRV
            (uiDirection ui)
            realCSelRV
            (constR $ readTVar engineTuningSTM)
            (constR engineFeedback)

    -- start limit monitor
    limitMonitorTask <- async $ do
        withConfig selectedPlayerRV (limitMonitor (uiLimits ui))

    -- start engine
    engineTask <- async $ do
        withConfig (uiTSet ui) $ \t0 -> do
            atomically $ writeTVar tUtc t0
            withConfig engineConfigRV $ \cfg -> do
                atomically $ do
                    uiConsole ui ConsoleClear
                    uiBufferLevel ui 0

                -- Initial delay, to prevent fast respawn during config change.
                -- Wait, until user makes final config selection, then start.
                threadDelaySec 1.0
                engine 10.0 tUtc cfg

    let periodicAction = do
            checkTask ui "limit monitor" limitMonitorTask
            checkTask ui "engine" engineTask

    -- return engine config RV and periodic action (to be run from GUI)
    pure (engineConfigRV, periodicAction)
