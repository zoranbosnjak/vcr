------------------
-- |
-- Module: Streams
--
-- IO streaming
--
-- > import Streams
-- > import Control.Concurrent
-- > import Control.Exception
-- > import Control.Monad hiding (forever)
-- > import Control.Monad.IO.Class
-- > import System.IO
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
-- >        acquire = liftIO $ do
-- >            print ("open file...", path)
-- >            openFile path ReadMode
-- >        release f = liftIO $ do
-- >            print ("close file...", path)
-- >            hClose f
-- >        action produce = bracket acquire release $ \f -> do
-- >            -- do something with a file handle 'f'
--
--      * create delay pipe
--
-- >    pip :: Pipe a a
-- >    pip = mkPipe $ \consume produce -> forever $ do
-- >        msg <- consume Clear
-- >        liftIO $ threadDelay 1000000
-- >        produce msg
--
--      * create consumer
--
-- >    cons :: Consumer Char
-- >    cons = mkConsumer $ \consume Clear -> do
-- >        replicateM_ 10 (consume >>= (liftIO . print))
--
--      * run the chain
--
-- >    main :: IO ()
-- >    main = runStream $ prod >-> pip >-> cons
--

{-# LANGUAGE ScopedTypeVariables #-}

module Streams
where

import           Control.Monad hiding (forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Exception
import           Control.Exception (SomeException)
import           Control.Concurrent.STM
import           Control.Concurrent.Async
                    (withAsync, cancel, wait, waitEitherCatch, race)
import           Control.Monad.Trans.Except

data Checkpoint
    = MoreData  -- consumer needs more data, do not cancel
    | Clear     -- it is safe to cancel consumer at this point
    deriving (Eq, Show)

type StreamT = ExceptT () IO
type ConsumeFunc a = Checkpoint -> StreamT a
type ProduceFunc b = b -> StreamT ()

-- | General streaming component.
data Streaming a b = Streaming
    { streamAction :: ConsumeFunc a -> ProduceFunc b -> StreamT ()
    }

type Producer a = Streaming () a
type Consumer a = Streaming a ()
type Pipe a b = Streaming a b
type Effect = Streaming () ()

-- | Forever implementation from Control.Monad in combination with transformers
-- has some problem with memory leak, use this version instead.
forever :: Monad m => m a -> m b
forever act = act >> forever act

-- | Chain 2 streaming components using TBQueue and async.
(>->) :: Streaming a b -> Streaming b c -> Streaming a c
(>->) a b = Streaming action where
    action consume produce = do
        -- create a channel between left and right component
        chan <- liftIO $ newTBQueueIO 1
        let act1 = streamAction a
            act2 = streamAction b
            -- when left action produces a value, it's written to chan
            produce1 x = liftIO $ atomically $ writeTBQueue chan $ Just x
            -- When right action requires a value, it's taken from chan.
            -- In case of "end of data" - Nothing, the consumer
            -- shall terminate gracefully, that is: only on the next
            -- call to 'consume' function.
            consume2 = \checkpoint -> do
                mVal <- liftIO $ atomically $ readTBQueue chan
                case mVal of
                    Nothing -> case checkpoint of
                        MoreData -> fail "broken pipe"
                        Clear -> throwE ()
                    Just val -> return val
        liftIO $ do
            -- run left and right actions and wait for any to terminate
            withAsync (runExceptT (act1 consume produce1)) $ \left -> do
              withAsync (runExceptT (act2 consume2 produce)) $ \right -> do
                -- in case of interrupt, terminate left action first
                -- and the right action shall terminate on it's own
                let onInterrupt (_e :: SomeException) = cancel left
                (flip Control.Exception.catch) onInterrupt $ do
                    rv <- waitEitherCatch left right
                    result <- case rv of
                        Left result -> do
                            -- If a producer terminates, indicate end of
                            -- data, then wait for the consumer to terminate
                            -- on it's own.
                            atomically $ writeTBQueue chan Nothing
                            _ <- wait right
                            return result
                            -- If a consumer terminates, cancel the producer.
                        Right result -> do
                            cancel left
                            return result
                    -- rethrow exception if any
                    case result of
                        Left e -> Control.Exception.throw e
                        Right _ -> return ()

-- | Run a stream.
runStream :: Effect -> IO ()
runStream s = runExceptT action >> return () where
    action = (streamAction s) noConsume noProduce

-- | Consumer that fails.
noConsume :: a
noConsume = error "can not consume values"

-- | Producer that fails.
noProduce :: t -> a
noProduce _ = error "can not produce values"

-- | Create producer. It's action can only produce values.
mkProducer :: (ProduceFunc a -> StreamT ()) -> Producer a
mkProducer f = Streaming action where
    action _consume produce = f produce

-- | Create pipe.
mkPipe :: (ConsumeFunc a -> ProduceFunc b -> StreamT ()) -> Pipe a b
mkPipe = Streaming

-- | Create effect.
mkEffect :: StreamT () -> Effect
mkEffect f = Streaming action where
    action _consume _produce = f

-- | Create consumer. It's action can only consume values.
mkConsumer :: (ConsumeFunc a -> StreamT ()) -> Consumer a
mkConsumer f = Streaming action where
    action consume _produce = f consume

-- | Connect Consumer and Producer back-to-back and form a Pipe.
mkBridge ::
    IO r
    -> (r -> IO c)
    -> (r -> Consumer a)    -- consumer
    -> (r -> Producer b)    -- producer
    -> (r -> Producer b)    -- final producer before release phase
    -> Pipe a b
mkBridge acquire release rx tx flush = Streaming action where
    action consume produce = bracket acquire release $ \r -> liftIO $ do
        let act1 = runExceptT $ (streamAction (rx r)) consume noProduce
            act2 = runExceptT $ (streamAction (tx r)) noConsume produce
            act3 = runExceptT $ (streamAction (flush r)) noConsume produce
        -- run consumer and producer,
        -- flush internal data if original consumer terminates
        rv <- race act1 act2
        case rv of
            Left _ -> act3 >> return ()
            Right _ -> return ()

-- | Utils

-- | map function over each element
map :: (a -> b) -> Pipe a b
map f = mkPipe $ \consume produce -> forever $ do
    consume Clear >>= produce . f

-- | filter stream elements
filter :: (a -> Bool) -> Pipe a a
filter f = mkPipe $ \consume produce -> forever $ do
    msg <- consume Clear
    when (f msg) $ produce msg

-- | pipe with internal buffer of size n
buffer :: Maybe Int -> Pipe a a
buffer mn = case mn of
    Nothing -> Streams.map id
    Just n -> case n <= 0 of
        True -> Streams.map id
        False -> mkBridge (newTBQueueIO n) (\_ -> return ()) rx tx flush
  where
    rx q = mkConsumer $ \consume -> forever $ do
        consume Clear >>= liftIO . atomically . writeTBQueue q
    tx q = mkProducer $ \produce -> forever $ do
        liftIO (atomically (readTBQueue q)) >>= produce
    flush q = mkProducer $ \produce -> do
        msgs <- liftIO $ atomically $ flushTBQueue q
        mapM_ produce msgs

-- | Producer that produces nothing.
empty :: Producer a
empty = mkProducer $ \_ -> return ()

-- | Consumer that just consumes all input.
drain :: Consumer a
drain = mkConsumer $ \consume -> forever $ do
    consume Clear >> return ()

-- | Produce each element from foldable.
fromFoldable :: Foldable t => t a -> Producer a
fromFoldable lst = mkProducer $ \produce -> do
    mapM_ produce lst

-- | Stream version of 'bracket'.
bracket :: IO a -> (a -> IO b)  -> (a -> StreamT c) -> StreamT c
bracket acquire release action = ExceptT $
    Control.Exception.bracket acquire release (runExceptT . action)

