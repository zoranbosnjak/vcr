------------------
-- |
-- Module: Streams
--
-- Stream processing
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Streams where

import           Data.Bool
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Exception (throw)
import           Control.Concurrent.STM
import           Control.Concurrent.Async

data Message a c
    = Message a
    | EndOfData c
    deriving (Eq, Show)

mapMessage :: (a -> b) -> Message a c -> Message b c
mapMessage f (Message a) = Message (f a)
mapMessage _f (EndOfData c) = EndOfData c

type ProduceFunc a c = a -> STM ()
type ConsumeFunc a c = STM (Message a c)

data Streaming a b c = Streaming
    { streamAction :: ConsumeFunc a c -> ProduceFunc b c -> IO c
    }

type Producer a = Streaming () a
type Consumer a = Streaming a ()
type Pipe a b = Streaming a b
type Effect = Streaming () ()

threadDelaySec :: Double -> IO ()
threadDelaySec = threadDelay . round . (1000000*)

-- | When one streaming component terminates, run another.
-- Similar to 'Alternative', without Fuctor, Applicative... constraint.
class After a where
    (-->) :: a -> a -> a

-- | When one producer exhausts, run another.
instance After (Producer a c) where
    (-->) p1 p2 = mkProducer $ \produce ->
        (streamAction p1) noConsume produce
        >> (streamAction p2) noConsume produce

-- | When one consumer does not want any more data, run another.
instance After (Consumer a c) where
    (-->) c1 c2 = mkConsumer $ \consume -> do
        chan <- newEmptyTMVarIO
        let consume1 = takeTMVar chan
            produce = putTMVar chan
            act1 = streamAction c1
            act2 = streamAction c2
        -- feed consumer c1 and peek messages
        withAsync (act1 consume1 noProduce) $ \a -> do
            fix $ \loop -> do
                eMsg <- atomically $
                    fmap Left (waitSTM a) `orElse` fmap Right consume
                case eMsg of
                    Left _ -> act2 consume noProduce -- run second consumer
                    Right val -> do
                        atomically $ produce val
                        case val of
                            EndOfData _ -> wait a   -- no more data, done
                            Message _ -> loop

-- | Run 2 effects in sequence.
instance After (Effect c) where
    (-->) e1 e2 = mkEffect $ do
        (streamAction e1) noConsume noProduce
        >> (streamAction e2) noConsume noProduce

-- | The (<>) operator for producers.
combineProducers :: Producer a c -> Producer a c -> Producer a c
combineProducers a b = mkProducer $ \produce -> do
    let act1 = streamAction a
        act2 = streamAction b
    withAsync (act1 noConsume produce) $ \leftSide -> do
        withAsync (act2 noConsume produce) $ \rightSide -> do
            waitEither leftSide rightSide >>= return . either id id

-- | Empty producer.
silence :: Producer a c
silence = mkProducer $ \_produce -> forever $ threadDelaySec 1

-- | Concat list of producers (parallel).
mergeProducers :: [Producer a c] -> Producer a c
mergeProducers [] = silence
mergeProducers (p1:p2:[]) = p1 `combineProducers` p2
mergeProducers (p:ps) = p `combineProducers` mergeProducers ps

-- | The (<>) operator for consumers.
combineConsumers :: Consumer a c -> Consumer a c -> Consumer a c
combineConsumers a b = mkConsumer $ \consume -> do
    chanA <- newEmptyTMVarIO
    chanB <- newEmptyTMVarIO
    let act1 = streamAction a
        act2 = streamAction b
        consumeA = takeTMVar chanA
        consumeB = takeTMVar chanB
        duplicate = forever $ atomically $ do
            x <- consume
            putTMVar chanA x
            putTMVar chanB x
    withAsync duplicate $ \_ -> do
        withAsync (act1 consumeA noProduce) $ \leftSide -> do
            withAsync (act2 consumeB noProduce) $ \rightSide -> do
                waitEither leftSide rightSide >>= \case
                    Left rv -> wait rightSide >> return rv
                    Right rv -> wait leftSide >> return rv

-- | stream loop
processMessage :: IO (Message a c) -> (c -> IO c) -> (a -> IO b) -> IO c
processMessage runConsume onEnd onData = fix $ \loop -> do
    runConsume >>= \case
        EndOfData rv -> onEnd rv
        Message msg -> do
            _ <- onData msg
            loop

-- | simple stream loop
processMessage_ :: IO (Message a c) -> (a -> IO b) -> IO c
processMessage_ runConsume onData = processMessage runConsume return onData

-- | Empty consumer
drain :: Consumer a c
drain = mkConsumer $ \consume ->
    processMessage_ (atomically consume) (\_ -> return ())

forkConsumers :: [Consumer a c] -> Consumer a c
forkConsumers [] = drain
forkConsumers (c1:c2:[]) = c1 `combineConsumers` c2
forkConsumers (c:cs) = c `combineConsumers` forkConsumers cs

-- | Producer function that fails.
noProduce :: t -> a
noProduce _ = error "can not produce values"

-- | Consumer function that fails.
noConsume :: a
noConsume = error "can not consume values"

-- | Create producer
mkProducer :: (ProduceFunc a c -> IO c) -> Producer a c
mkProducer f = Streaming $ \_consume produce -> f produce

-- | Create consumer
mkConsumer :: (ConsumeFunc a c -> IO c) -> Consumer a c
mkConsumer f = Streaming $ \consume _produce -> f consume

-- | Create pipe
mkPipe :: (ConsumeFunc a c -> ProduceFunc b c -> IO c) -> Pipe a b c
mkPipe = Streaming

-- | Create effect.
mkEffect :: IO c -> Effect c
mkEffect f = Streaming $ \_consume _produce -> f

-- | Run a stream.
runStream :: Effect c -> IO c
runStream s = (streamAction s) noConsume noProduce

runStream_ :: Effect c -> IO ()
runStream_ s = runStream s >> return ()

-- | Chain 2 streaming components.
(>->) :: Streaming a b c -> Streaming b d c -> Streaming a d c
(>->) a b = Streaming action where
    action consume produce = do
        chan <- newEmptyTMVarIO
        let act1 = streamAction a
            act2 = streamAction b
            produce1 = putTMVar chan . Message
            consume2 = takeTMVar chan
        withAsync (act1 consume produce1) $ \leftSide -> do
            withAsync (act2 consume2 produce) $ \rightSide -> do
                waitEitherCatch leftSide rightSide >>= \case
                    -- producer finished
                    Left rv -> case rv of
                        Left e -> throw e
                        Right val -> do
                            atomically $ putTMVar chan $ EndOfData val
                            wait rightSide
                    -- consumer finished
                    Right rv -> case rv of
                        Left e -> throw e
                        Right val -> return val

-- | Helper function for creating pipes.
mapEachPipe :: (ProduceFunc b c -> a -> STM ()) -> Pipe a b c
mapEachPipe f = mkPipe $ \consume produce ->
    processMessage_ (atomically consume) $ \msg -> atomically (f produce msg)

-- | a version of map that can remove elements
mapMaybe :: (a -> Maybe b) -> Pipe a b c
mapMaybe f = mapEachPipe $ \produce a -> case f a of
    Nothing -> return ()
    Just b -> produce b

map :: (a -> b) -> Pipe a b c
map f = mapMaybe $ \x -> Just (f x)

filter :: (a -> Bool) -> Pipe a a c
filter f = mapMaybe $ \x -> bool Nothing (Just x) (f x)

-- | Produce each element from foldable.
fromFoldable :: (Foldable t) => t a -> Producer a ()
fromFoldable lst = mkProducer $ \produce -> do
    mapM_ (atomically . produce) lst

consumeToIO :: (a -> IO b) -> Consumer a c
consumeToIO act = mkConsumer $ \consume ->
    processMessage_ (atomically consume) act

-- | Create statefull pipe with IO action
mkGenPipeIO :: g -> (a -> g -> IO (b, g)) -> Pipe a b c
mkGenPipeIO initial f = mkPipe $ loop initial where
    loop g consume produce = do
        atomically consume >>= \case
            EndOfData rv -> return rv
            Message val -> do
                (b, g') <- f val g
                _ <- atomically $ produce b
                loop g' consume produce

-- | Filter each element inside IO
filterIO :: (a -> IO (Maybe b)) -> Pipe a b c
filterIO f = mkPipe $ \consume produce ->
    processMessage_ (atomically consume) $ \x -> do
        f x >>= \case
            Nothing -> return ()
            Just y -> atomically $ produce y

-- | Check that every message leaves the pipe a within given dt (seconds).
expedite :: Double -> IO () -> Pipe a a c
expedite dt onProblem = mkPipe $ \consume produce ->
    processMessage_ (atomically consume) $ \msg -> race_
        (atomically $ produce msg)
        (threadDelaySec dt >> onProblem)

