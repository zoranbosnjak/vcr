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

import           Control.Concurrent.STM.TQueue (flushTQueue)
import           Data.Bool
import           UnliftIO

import           Data.CBMVar
import           Data.CBMVar.Reactive
import           Data.ReactiveValue

-- | Setup callback, but also run it initially, to make the first update.
follow :: (ReactiveValueRead a b m) => a -> (b -> m ()) -> m ()
follow val act = do
    let doUpdate = reactiveValueRead val >>= act
    doUpdate
    reactiveValueOnCanRead val doUpdate

-- | Lift a transformation, create a new RV.
liftR4 ::
    ( ReactiveValueRead p1 t1 m, ReactiveValueRead p2 t2 m
    , ReactiveValueRead p3 t3 m, ReactiveValueRead p4 t4 m)
    => (t2 -> t3 -> t1 -> t4 -> a) -> p2 -> p3 -> p1 -> p4
    -> ReactiveFieldRead m a
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
    ( ReactiveValueRead p1 t1 m, ReactiveValueRead p2 t2 m
    , ReactiveValueRead p3 t3 m, ReactiveValueRead p4 t4 m
    , ReactiveValueRead p5 t5 m)
    => (t3 -> t2 -> t4 -> t1 -> t5 -> a) -> p3 -> p2 -> p4 -> p1 -> p5
    -> ReactiveFieldRead m a
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
    ( ReactiveValueRead p1 t1 m, ReactiveValueRead p2 t2 m
    , ReactiveValueRead p3 t3 m, ReactiveValueRead p4 t4 m
    , ReactiveValueRead p5 t5 m, ReactiveValueRead p6 t6 m)
    => (t3 -> t4 -> t2 -> t5 -> t1 -> t6 -> a) -> p3 -> p4 -> p2 -> p5 -> p1 -> p6
    -> ReactiveFieldRead m a
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
