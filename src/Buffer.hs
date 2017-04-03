----------------
-- |
-- Module:      Buffer
--
-- Buffer manipulations
--

module Buffer
    ( Buffer
    , Thrashold (thLength, thBytes, thSeconds)
    , thrashold
    , isBelow

    -- buffer manipulation
    , newBuffer
    , appendBuffer
    , readBuffer

    -- helpers
    , kiloMega
    , anyLimit
    ) where

import Control.Concurrent.STM
import Data.Foldable
import Data.Maybe
import Data.Sequence as DS
import Options.Applicative
import System.Clock

import Event

data Buffer = Buffer
    { bufData       :: TVar (Seq Event)
    , bufAppendTh   :: Thrashold
    , bufReadTh     :: Thrashold
    }

data Thrashold = Thrashold
    { thLength      :: Maybe Int
    , thBytes       :: Maybe Integer
    , thSeconds     :: Maybe Double
    } deriving (Eq, Show)

instance Monoid Thrashold where
    mempty = Thrashold Nothing Nothing Nothing
    Thrashold a1 b1 c1 `mappend` Thrashold a2 b2 c2 = Thrashold
        (lower a1 a2)
        (lower b1 b2)
        (lower c1 c2)
      where
        lower Nothing x = x
        lower x Nothing = x
        lower (Just a) (Just b) = Just (min a b)

-- | Make sure that th1 (readTh) will react before th2 (appendTh), equal is OK.
isBelow :: Thrashold -> Thrashold -> Bool
isBelow (Thrashold a1 b1 c1) (Thrashold a2 b2 c2) = and [a1<!a2, b1<!b2, c1<!c2]
  where
    Just a <! Just b = a <= b
    _ <! Nothing = True
    Nothing <! _ = False

thrashold :: (Maybe Int) -> (Maybe Integer) -> (Maybe Double) -> Thrashold
thrashold = Thrashold

anyLimit :: Thrashold -> Bool
anyLimit (Thrashold a b c) = or [isJust a, isJust b, isJust c]

-- | Helper function to convert eg. 1k -> 1024
kiloMega :: ReadM Integer
kiloMega = eitherReader $ \arg -> do
    let (a,b) = case last arg of
            'k' -> (init arg, 1)
            'M' -> (init arg, 2)
            'G' -> (init arg, 3)
            _ -> (arg, 0)
    case reads a of
        [(r, "")] -> return (r * (1024^(b::Int)))
        _         -> Left $ "cannot parse value `" ++ arg ++ "'"

-- | Create new buffer.
newBuffer :: Thrashold -> Thrashold -> STM Buffer
newBuffer appendTh readTh = Buffer
    <$> newTVar mempty
    <*> pure appendTh
    <*> pure readTh

-- | Append new events to the buffer.
-- We need to respect buffer append thrashold, so in case of
-- overflow, some events will not be appended (but returned).
appendBuffer :: TimeSpec -> Buffer -> [Event] -> STM [Event]
appendBuffer ts buf evts = do
    content <- readTVar $ bufData buf
    let (newContent, leftover) = appendItems content evts
    writeTVar (bufData buf) newContent
    return leftover
  where
    appendItems content [] = (content, [])
    appendItems content lst@(x:xs) =
        let newContent = content `mappend` singleton x
        in case thCompare ts newContent (bufAppendTh buf) of
            GT -> (content, lst)
            _ -> appendItems newContent xs

-- | Read buffer content when ready to read.
-- Read only as many events as allowed by the thrashold
readBuffer :: TVar TimeSpec -> Buffer -> STM [Event]
readBuffer tick buf = do
    ts <- readTVar tick
    content <- readTVar $ bufData buf
    (contentReady, leftover) <- readItems ts DS.empty content
    writeTVar (bufData buf) leftover
    return $ toList contentReady
  where
    readItems ts acc content = case viewr content of
        EmptyR -> retry
        (rest :> aR) ->
            let newAcc = aR <| acc
            in case thCompare ts newAcc (bufReadTh buf) of
                LT -> readItems ts newAcc rest
                _ -> return (newAcc, rest)

-- | Compare content of the buffer with the thrashold
thCompare :: TimeSpec -> Seq Event -> Thrashold -> Ordering
thCompare ts s th = maximum [thCompareLength, thCompareBytes, thCompareSeconds]
  where
    mCompare _ Nothing = LT
    mCompare actual (Just limit) = compare actual limit

    thCompareLength = mCompare (DS.length s) (thLength th)
    thCompareBytes = mCompare (foldr (+) 0 (fmap sizeOf s)) (thBytes th)
    thCompareSeconds = mCompare age (thSeconds th) where
        oldest = viewr s
        age = case oldest of
            (_ :> aR) -> secondsSince (eMonoTime aR) ts
            _ -> 0
        secondsSince :: TimeSpec -> TimeSpec -> Double
        secondsSince t1 t2 = (/ (10^(9::Int))) $ fromIntegral (t2' - t1') where
            t2' = toNanoSecs t2
            t1' = toNanoSecs t1

