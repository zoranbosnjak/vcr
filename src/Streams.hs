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
-- >        msg <- consume
-- >        liftIO $ threadDelay 1000000
-- >        produce msg
--
--      * create consumer
--
-- >    cons :: Consumer Char
-- >    cons = mkConsumer $ \consume -> do
-- >        replicateM_ 10 (consume >>= (liftIO . print))
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
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Exception
import           Control.Concurrent.STM
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Trans.Except

type StreamT = ExceptT () IO
type ConsumeFunc a = StreamT a
type ProduceFunc b = b -> StreamT ()

-- | General streaming component.
data Streaming a b = Streaming
    { streamAction :: ConsumeFunc a -> ProduceFunc b -> StreamT ()
    }

type Producer a = Streaming () a
type Consumer a = Streaming a ()
type Pipe a b = Streaming a b
type Effect = Streaming () ()

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
            consume2 = do
                mVal <- liftIO $ atomically $ readTBQueue chan
                case mVal of
                    Nothing -> throwE ()
                    Just val -> return val
        liftIO $ do
            -- run left and right actions and wait for any to terminate
            left  <- Async.async $ runExceptT (act1 consume produce1)
            right <- Async.async $ runExceptT (act2 consume2 produce)
            rv <- Async.waitEitherCatch left right
            result <- case rv of
                Left result -> do
                    -- If a producer terminates, indicate end of
                    -- data, then wait for the consumer to terminate
                    -- on it's own.
                    atomically $ writeTBQueue chan Nothing
                    _ <- Async.wait right
                    return result
                    -- If a consumer terminates, cancel the producer.
                Right result -> do
                    Async.cancel left
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
mkProducer :: (ProduceFunc b -> StreamT ()) -> Streaming a b
mkProducer f = Streaming action where
    action _consume produce = f produce

-- | Create pipe.
mkPipe :: (ConsumeFunc a -> ProduceFunc b -> StreamT ()) -> Streaming a b
mkPipe = Streaming

-- | Create consumer. It's action can only consume values.
mkConsumer :: (ConsumeFunc a -> StreamT ()) -> Streaming a b
mkConsumer f = Streaming action where
    action consume _produce = f consume

-- | Combine multiple producers of the same type to a single producer.
mergeStreams :: [Producer a] -> Producer a
mergeStreams lst = mkProducer action where
    action produce = do
        -- Start original producers from the list, such that each
        -- instance is using the same outer 'produce' function.
        producers <- forM lst $ \producer -> liftIO $ do
            Async.async $ runExceptT $ (streamAction producer) noConsume produce
        -- Finish action when ALL producers are done.
        liftIO $ mapM_ Async.wait producers

-- | Fork data to all consumers on the list.
forkStreams :: [Consumer a] -> Consumer a
forkStreams lst = mkConsumer action where
    action consume = do
        branches <- forM lst $ \consumer -> do
            -- need additional channel for each consumer
            chan <- liftIO $ newTBQueueIO 1
            -- need also a producer that would forward
            -- messages from the channel to the original consumer
            let producer = mkProducer $ \produce -> forever $ do
                    val <- liftIO $ atomically $ readTBQueue chan
                    produce val
            a <- liftIO $ Async.async $ runStream $ producer >-> consumer
            return (chan, a)

        let dispatch = do
                x <- consume
                writers <- forM branches $ \(chan,_a) -> do
                    liftIO $ Async.async $ atomically $ writeTBQueue chan x
                writersDone <- liftIO $ Async.async $ mapM_ Async.wait writers
                someTermination <- liftIO $ Async.async $ Async.waitAny $
                    (snd <$> branches)
                rv <- liftIO $ Async.waitEitherCancel
                    someTermination
                    writersDone
                case rv of
                    -- message is fully consumed, make another iterration
                    Right _ -> dispatch

                    -- in case of any consumer termination,
                    -- we are done with dispatch, just do the cleanup
                    Left _ -> do
                        liftIO $ mapM_ Async.cancel (snd <$> branches)
                        liftIO $ mapM_ Async.cancel writers
        dispatch

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
onTerminate :: StreamT () -> Streaming a b -> Streaming a b
onTerminate act s = Streaming action where
    action consume produce = do
        a <- liftIO $ Async.async $ runExceptT $
            (streamAction s) consume produce
        rv <- liftIO $ Async.waitCatch a
        case rv of
            -- exception
            Left e -> do
                act
                Control.Exception.throw e
            -- function returned something
            Right (Right _) -> act
            -- endo of data (do not call action)
            Right (Left ()) -> return ()

-- | Consumer that just consumes all input.
drain :: Consumer a
drain = mkConsumer $ \consume -> forever $ do
    consume >> return ()

-- | Produce each element from foldable.
fromFoldable :: Foldable t => t a -> Producer a
fromFoldable lst = mkProducer $ \produce -> do
    mapM_ produce lst

-- | Stream version of bracket.
bracket ::
    StreamT a
    -> (a -> StreamT b)
    -> (a -> StreamT c)
    -> StreamT c
bracket acquire release act =
    liftIO $ Control.Exception.bracket acquire' release' act'
  where
    acquire' = do
        rv <- runExceptT acquire
        case rv of
            Left _ -> fail "acquire exception"
            Right val -> return val
    release' res = do
        rv <- runExceptT $ release res
        case rv of
            Left _ -> fail "release exception"
            Right val -> return val
    act' res = do
        rv <- runExceptT $ act res
        case rv of
            Left _ -> fail "action exception"
            Right val -> return val

