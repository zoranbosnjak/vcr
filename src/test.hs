
import Control.Concurrent.STM
import Test.QuickCheck

import Buffer

main :: IO ()
main = do
    events <- generate $ infiniteListOf arbitrary

    -- append nothing
    do
        b <- atomically $ newBuffer mempty mempty
        err <- atomically $ appendBuffer b []
        print $ length err

    -- append one event
    do
        b <- atomically $ newBuffer mempty mempty
        err <- atomically $ appendBuffer b $ take 1 events
        print $ length err

    -- append 100 events
    do
        b <- atomically $ newBuffer mempty mempty
        err <- atomically $ appendBuffer b $ take 100 events
        print $ length err

    -- append 20 events, limit to 5
    do
        b <- atomically $ newBuffer (mempty {maxEvents = Just 5}) mempty
        err <- atomically $ appendBuffer b (take 20 events)
        print $ length err

    return ()

