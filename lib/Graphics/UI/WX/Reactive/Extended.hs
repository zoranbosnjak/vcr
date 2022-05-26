{-# LANGUAGE FlexibleContexts #-}

-- | This module extends 'Graphics.UI.WX.Reactive' module with various widgets.

module Graphics.UI.WX.Reactive.Extended
    -- re-exports
    ( module Graphics.UI.WX.Reactive
    -- own definitions
    , module Graphics.UI.WX.Reactive.Extended
    ) where

import           UnliftIO
import           Control.Monad (when)

import           Graphics.UI.WX
import           Graphics.UI.WX.Reactive

import           Data.ReactiveValue.Extended

-- | Reactive selector wrapper, suppress event propagation on the same value.
selectorR :: (Selecting w, Selection w, Enum a) => w
    -> IO (ReactiveFieldRead IO a)
selectorR ctr = do
    notifiers <- newMVar []
    oldValue <- get ctr selection >>= newMVar
    set ctr
        [ on select := do
            y <- get ctr selection
            x <- modifyMVar oldValue (\a -> return (y, a))
            Control.Monad.when (y /= x) $ do
                readMVar notifiers >>= sequence_
        ]
    let getter = toEnum <$> get ctr selection
        notifier p = modifyMVar_ notifiers (\x -> return (x ++ [p]))
    return $ ReactiveFieldRead getter notifier

-- | Reactive wrapper for checkable widget.
checkableR :: (Commanding w, Checkable w) => w -> IO (ReactiveFieldRead IO Bool)
checkableR ctr = do
    notifiers <- newMVar []
    set ctr [ on command :~ \act -> act >> (readMVar notifiers >>= sequence_) ]
    let getter = get ctr checked
        notifier p = modifyMVar_ notifiers (\x -> return (x ++ [p]))
    return $ ReactiveFieldRead getter notifier

-- | Sync control enabled state from reactive value.
setEnabled :: (ReactiveValueRead a1 a2 IO, Able w, Eq a2) =>
    a1 -> w -> a2 -> IO ()
setEnabled a ctr enabledVal = follow a $ \x -> do
    set ctr [ enabled := (x == enabledVal) ]

