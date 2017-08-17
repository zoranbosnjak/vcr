------------------
-- |
-- Module: Streams
--
-- IO streaming
--
-- > import Streams
-- > import Control.Concurrent
-- > import Control.Exception
-- > import Control.Monad
--
-- Examples:
--
--      * create simple producer
--
-- >    prod :: Producer Char
-- >    prod = mkProducer $ \produce -> forever $ do
-- >        produce 'x'
--
--      * producer with resource handling
--
-- >    srcFile :: FileName -> Producer String
-- >    srcFile path = mkProducer action where
-- >        acquire = do
-- >            print "open file..."
-- >            TODO...
-- >        release f = do
-- >            print "close file..."
-- >            TODO...
-- >        action produce = bracket acquire release $ \f -> do
-- >            TODO...
--
--      * create delay pipe
--
-- >    pip :: Pipe a a
-- >    pip = mkPipe $ \consume produce -> forever $ do
-- >        msg <- consume
-- >        threadDelay 1000000
-- >        produce msg
--
--      * create consumer
--
-- >    cons :: Consumer Char
-- >    cons = mkConsumer $ \consume -> do
-- >        replicateM_ 10 (consume >>= print)
--
--      * run the chain
--
-- >    main :: IO ()
-- >    main = runStream $ prod >-> pip >-> cons
--
--      * more complex example
--
-- >    main :: IO ()
-- >    main = runStream $
-- >        mergeStreams [prod1 >-> p1, prod2, ...]
-- >        >-> somePipe
-- >        >-> forkStreams [cons1, p3 >-> cons2]
--

module Streams
where

import           Control.Monad
import           Control.Exception (bracket, throw)
import qualified Control.Concurrent
import qualified Control.Concurrent.Async

type ConsumeFunc a = IO a
type ProduceFunc b = b -> IO ()

-- | General streaming component.
data Streaming a b = Streaming
    { streamAction :: ConsumeFunc a -> ProduceFunc b -> IO ()
    }

type Producer a = Streaming () a
type Consumer a = Streaming a ()
type Pipe a b = Streaming a b
type Effect = Streaming () ()

-- | Create producer. It's action can only produce values.
mkProducer :: (ProduceFunc b -> IO ()) -> Streaming a b
mkProducer f = Streaming action where
    action _consume produce = f produce

-- | Create pipe.
mkPipe :: (ConsumeFunc a -> ProduceFunc b -> IO ()) -> Streaming a b
mkPipe = Streaming

-- | Create consumer. It's action can only consume values.
mkConsumer :: (ConsumeFunc a -> IO ()) -> Streaming a b
mkConsumer f = Streaming action where
    action consume _produce = f consume

-- | Chain 2 streaming components using MVar and async.
(>->) :: Streaming a b -> Streaming b c -> Streaming a c
(>->) a b = Streaming action where
    action consume produce = do
        var <- Control.Concurrent.newEmptyMVar
        let act1 = streamAction a
            act2 = streamAction b
            produce1 x = Control.Concurrent.putMVar var x
            consume2 = Control.Concurrent.takeMVar var
        Control.Concurrent.Async.race
            (act1 consume produce1)
            (act2 consume2 produce)
        >> return ()

-- | Combine multiple producers of the same type to a single producer.
mergeStreams :: [Producer a] -> Producer a
mergeStreams lst = mkProducer action where
    action produce = bracket acquire release $ \producers -> do
        _ <- Control.Concurrent.Async.waitAnyCancel producers
        return ()
      where
        -- start original producers from the list
        acquire = forM lst $ \p -> do
            Control.Concurrent.Async.async $ (streamAction p) noConsume produce
        -- stop producers when done
        release = mapM_ Control.Concurrent.Async.cancel

-- | Feed data to all consumers on the list.
forkStreams :: [Consumer a] -> Consumer a
forkStreams lst = mkConsumer action where
    action consume = bracket acquire release $ \consumers -> do
        dispatcher <- Control.Concurrent.Async.async $ forever $ do
            msg <- consume
            forM_ consumers $ \(var,_) -> do
                Control.Concurrent.putMVar var msg
        _ <- Control.Concurrent.Async.waitAnyCancel $
            dispatcher : (snd <$> consumers)
        return ()
      where
        acquire = forM lst $ \p -> do
            -- separate var for each consumer
            var <- Control.Concurrent.newEmptyMVar
            a <- Control.Concurrent.Async.async $
                (streamAction p) (Control.Concurrent.takeMVar var) noProduce
            return (var, a)
        release consumers = forM_ consumers $ \(_,a) -> do
            Control.Concurrent.Async.cancel a

-- | Run a stream.
runStream :: Effect -> IO ()
runStream s = action where
    action = (streamAction s) noConsume noProduce

-- | Consumer that fails.
noConsume :: a
noConsume = error "can not consume values"

-- | Producer that fails.
noProduce :: t -> a
noProduce _ = error "can not produce values"

-- | Utils

-- | map function over each element
map :: (a -> b) -> Streaming a b
map f = mkPipe $ \consume produce -> forever $ do
    consume >>= produce . f

-- | filter stream elements
filter :: (a -> Bool) -> Streaming a a
filter f = mkPipe $ \consume produce -> forever $ do
    msg <- consume
    when (f msg) $ produce msg

-- | Perform some action when original stream component terminates on it's own.
onTerminate :: IO () -> Streaming a b -> Streaming a b
onTerminate act s = Streaming action where
    action consume produce = do
        a <- Control.Concurrent.Async.async $
            (streamAction s) consume produce
        rv <- Control.Concurrent.Async.waitCatch a
        -- perform required action, then re-throw exception if any
        act
        case rv of
            Left e -> throw e
            Right _ -> return ()

-- | Consumer that just consumes all input.
drain :: Consumer a
drain = mkConsumer $ \consume -> forever $ do
    consume >> return ()

