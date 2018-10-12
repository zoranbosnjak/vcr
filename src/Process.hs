------------------
-- |
-- Module: Process
--
-- Run processes support routines
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module Process where

import           Control.Monad
import           Control.Monad.Fix
import qualified Control.Exception as Ex
import           Control.Concurrent.STM hiding (check)
import qualified Control.Concurrent.Async as Async
import           System.Log.Logger (Priority(INFO, NOTICE, ERROR))

import           Common (logM, threadDelaySec)

data Message a
    = Message a
    | EndOfData
    deriving (Eq, Show)

data Producer a = Producer { getProducer :: (a -> STM ()) -> IO () }

mapProducer :: (a -> b) -> Producer a -> Producer b
mapProducer f producer = Producer $ \produce -> do
    let consumer = Consumer $ untilData (atomically . produce . f)
    producer >-> consumer

data Consumer a = Consumer { getConsumer :: (STM (Message a)) -> IO () }

mapConsumer :: (b -> a) -> Consumer a -> Consumer b
mapConsumer f consumer = Consumer $ \consume -> do
    let producer = Producer $ \produce -> fix $ \loop -> do
            atomically consume >>= \case
                EndOfData -> return ()
                Message x -> (atomically $ produce $ f x) >> loop
    producer >-> consumer

data Process = forall a. Process
    { procName :: String
    , procAcquire :: IO a
    , procRelease :: a -> IO ()
    , procAction  :: a -> IO ()
    }

process :: String -> IO a -> (a -> IO ()) -> (a -> IO ()) -> Process
process = Process

process_ :: String -> IO () -> Process
process_ name act = Process name
    (return ())
    (\_ -> return ())
    (\_ -> act)

-- | Wait forever.
doNothing :: IO ()
doNothing = threadDelaySec 1 >> doNothing

-- | Run all processes from the list.
runAll :: [Process] -> IO ()
runAll [] = doNothing
runAll processes = do
    -- Acquire resources and prepare (not run) actions.
    lst1 <- forM processes $ \(Process name a b c) -> do
        res <- a
        let p = Async.async $ do
                (c res) `Ex.onException` (b res)
                b res
        return (p, name)
    -- Run actions only after all resources are acquired.
    lst2 <- forM lst1 $ \(asyncProc, name) -> do
        logM INFO $ "Starting " ++ show name
        p <- asyncProc
        return (p, name)
    -- As soon as one terminates, cancel them all.
    (a, rv) <- Async.waitAnyCatchCancel (fmap fst lst2)
    case lookup a lst2 of
        Nothing -> logM ERROR "Unknown process terminated"
        Just pn -> case rv of
            Left e -> logM NOTICE $
                show pn ++ " terminated with exception: " ++ show e
            Right _ -> logM INFO $
                show pn ++ " finished"

-- | Restart process when variable value changes.
withVar :: (Eq t, Show t) => String -> STM t -> (t -> IO ()) -> IO ()
withVar name getVar act = do
    initial <- atomically getVar
    loop initial
  where
    loop val = do
        logM INFO $ "(re)starting " ++ show name ++ ", " ++ show val
        rv <- Async.race (act val) (atomically $ do
            newVal <- getVar
            when (newVal == val) retry
            return newVal
            )
        case rv of
            Left _ -> return ()
            Right newVal -> loop newVal

-- | Run flow of producers and consumers.
runFlow :: [Producer a] -> [Consumer a] -> IO ()
runFlow producers consumers = do
    -- common broadcast channel
    hub <- newBroadcastTChanIO
    let produce = writeTChan hub . Message

    -- start consumers first
    consumers' <- forM consumers $ \consumer -> do
        ch <- atomically $ dupTChan hub
        let consume = readTChan ch
        Async.async (getConsumer consumer $ consume)

    -- then start producers
    producers' <- forM producers $ \producer -> do
        Async.async (getProducer producer $ produce)

    let flushAndWait = do
            mapM_ Async.cancel producers'
            atomically $ writeTChan hub EndOfData
            Async.mapConcurrently_ Async.wait consumers'
        forceStop e = do
            mapM_ Async.cancel producers'
            mapM_ Async.cancel consumers'
            Ex.throw e

    -- run until something happen
    leftSide <- Async.async $ Async.waitAnyCatchCancel producers'
    rightSide <- Async.async $ Async.waitAnyCatch consumers'
    Async.waitEitherCatch leftSide rightSide >>= \case
        Left rv -> do
            logM INFO "Producer finished"
            case rv of
                Left e -> forceStop e
                Right _ -> flushAndWait
        Right rv -> do
            logM INFO "Consumer finished"
            case rv of
                Left e -> forceStop e
                Right _ -> flushAndWait

connect :: Producer a -> Consumer a -> IO ()
connect a b = runFlow [a] [b]

(>->) :: Producer a -> Consumer a -> IO ()
(>->) = connect

untilData :: (t -> IO a) -> STM (Message t) -> IO ()
untilData act consume = fix $ \loop -> do
    atomically consume >>= \case
        EndOfData -> return ()
        Message msg -> act msg >> loop

drain :: STM (Message t) -> IO ()
drain = untilData (\_ -> return ())

