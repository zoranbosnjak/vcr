------------------
-- |
-- Module: Process
--
-- Run processes support routines
--

module Process where

import           Control.Monad (when)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM hiding (check)
import qualified Control.Concurrent.Async as Async
import           System.Log.Logger (Priority(INFO, NOTICE, ERROR))

import           Common (logM)

data Process = Process
    { procName :: String
    , procAction :: IO ()
    }

threadDelaySec :: Double -> IO ()
threadDelaySec = threadDelay . round . (1000000*)

-- | Wait forever.
doNothing :: IO ()
doNothing = threadDelaySec 1 >> doNothing

-- | Runn all processes from the list.
runAll :: [Process] -> IO ()
runAll [] = doNothing
runAll processes = do
    lst <- mapM runProc processes
    (a, rv) <- Async.waitAnyCatchCancel (fmap fst lst)
    case lookup a lst of
        Nothing -> logM ERROR "Unknown process terminated"
        Just pn -> case rv of
            Left e -> logM NOTICE $
                show pn ++ " terminated with exception: " ++ show e
            Right _ -> logM INFO $
                show pn ++ " finisned"
  where
    runProc (Process name act) = do
        logM INFO $ "Starting " ++ show name
        a <- Async.async act
        return (a, name)

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

