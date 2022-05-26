{-# LANGUAGE FlexibleContexts #-}

-- | This module extends 'Data.ReactiveValue' module with some
-- missing functions.

module Data.ReactiveValue.Extended
    -- re-exports
    ( module Data.ReactiveValue
    , module Data.CBMVar
    , module Data.CBMVar.Reactive
    -- own definitions
    , module Data.ReactiveValue.Extended
    ) where

import           Data.Bool
import           UnliftIO
import           Control.Concurrent.STM.TQueue (flushTQueue)

import           Data.ReactiveValue
import           Data.CBMVar
import           Data.CBMVar.Reactive

-- | Setup callback, but also run it initially, to make the first update.
follow :: (ReactiveValueRead a b m) => a -> (b -> m ()) -> m ()
follow val act = do
    let doUpdate = reactiveValueRead val >>= act
    doUpdate
    reactiveValueOnCanRead val doUpdate

-- | Lift a transformation, create a new RV.
liftR4 ::
    ( ReactiveValueRead a1 b1 m
    , ReactiveValueRead a1 a5 m, ReactiveValueRead a2 a6 m
    , ReactiveValueRead a3 a7 m, ReactiveValueRead a4 a8 m) =>
    (a5 -> a6 -> a7 -> a8 -> a9)
    -> a1 -> a2 -> a3 -> a4 -> ReactiveFieldRead m a9
liftR4 f e1 e2 e3 e4 = ReactiveFieldRead getter notifier
  where
    getter = f
        <$> reactiveValueRead e1
        <*> reactiveValueRead e2
        <*> reactiveValueRead e3
        <*> reactiveValueRead e4
    notifier p = do
        reactiveValueOnCanRead e1 p
        reactiveValueOnCanRead e2 p
        reactiveValueOnCanRead e3 p
        reactiveValueOnCanRead e4 p

-- | Lift a transformation, create a new RV.
liftR5 ::
    ( ReactiveValueRead a1 b1 m
    , ReactiveValueRead a1 a6 m
    , ReactiveValueRead a2 a7 m, ReactiveValueRead a3 a8 m
    , ReactiveValueRead a4 a9 m, ReactiveValueRead a5 a10 m) =>
    (a6 -> a7 -> a8 -> a9 -> a10 -> a11)
    -> a1 -> a2 -> a3 -> a4 -> a5 -> ReactiveFieldRead m a11
liftR5 f e1 e2 e3 e4 e5 = ReactiveFieldRead getter notifier
  where
    getter = f
        <$> reactiveValueRead e1
        <*> reactiveValueRead e2
        <*> reactiveValueRead e3
        <*> reactiveValueRead e4
        <*> reactiveValueRead e5
    notifier p = do
        reactiveValueOnCanRead e1 p
        reactiveValueOnCanRead e2 p
        reactiveValueOnCanRead e3 p
        reactiveValueOnCanRead e4 p
        reactiveValueOnCanRead e5 p

-- | Lift a transformation, create a new RV.
liftR6 ::
    ( ReactiveValueRead a1 b1 m
    , ReactiveValueRead a1 a7 m, ReactiveValueRead a2 a8 m
    , ReactiveValueRead a3 a9 m, ReactiveValueRead a4 a10 m
    , ReactiveValueRead a5 a11 m, ReactiveValueRead a6 a12 m) =>
    (a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13)
    -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> ReactiveFieldRead m a13
liftR6 f e1 e2 e3 e4 e5 e6 = ReactiveFieldRead getter notifier
  where
    getter = f
        <$> reactiveValueRead e1
        <*> reactiveValueRead e2
        <*> reactiveValueRead e3
        <*> reactiveValueRead e4
        <*> reactiveValueRead e5
        <*> reactiveValueRead e6
    notifier p = do
        reactiveValueOnCanRead e1 p
        reactiveValueOnCanRead e2 p
        reactiveValueOnCanRead e3 p
        reactiveValueOnCanRead e4 p
        reactiveValueOnCanRead e5 p
        reactiveValueOnCanRead e6 p

-- | Restart process on reactive value change.
withConfig :: (ReactiveValueRead a t IO) => a -> (t -> IO b) -> IO b
withConfig cfg action = do
    q <- newTQueueIO
    reactiveValueOnCanRead cfg $ do
        val <- reactiveValueRead cfg
        atomically $ writeTQueue q val
    reactiveValueRead cfg >>= loop q
  where
    getConfig q = atomically $ do
        isEmptyTQueue q
        >>= bool (last <$> flushTQueue q) retrySTM
    loop q val = race (getConfig q) (action val)
        >>= either (loop q) pure

