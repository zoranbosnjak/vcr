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
import qualified Control.Exception as Ex
import           Control.Concurrent.STM hiding (check)
import qualified Control.Concurrent.Async as Async
import           System.Log.Logger (Priority(INFO, NOTICE, ERROR))

import           Common (logM, threadDelaySec)

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

