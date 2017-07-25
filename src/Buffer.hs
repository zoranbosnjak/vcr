----------------
-- |
-- Module:      Buffer
--
-- Buffer manipulations
--

module Buffer {-
( Buffer
, Threshold (thLength, thBytes, thSeconds)
, threshold, thresholdOptions
, isBelow

-- buffer manipulation
, newBuffer
, appendBuffer
, readBuffer

-- helpers
, anyLimit
) -} where

import qualified Control.Concurrent.STM as STM
import           Data.Foldable (toList)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import qualified Data.Sequence as DS
import qualified Options.Applicative as Opt
import           System.Clock (TimeSpec, toNanoSecs)

import qualified Event

{-
-- | Buffer (sequence of data) with thresholds.
data Buffer = Buffer
    { bufData       :: STM.TVar (DS.Seq Event.Event)
    , bufAppendTh   :: Threshold
    , bufReadTh     :: Threshold
    }

-- | Threshold, limit by different properties.
data Threshold = Threshold
    { thLength      :: Maybe Int
    , thBytes       :: Maybe Integer
    , thSeconds     :: Maybe Double
    } deriving (Eq, Show)

instance Monoid Threshold where
    mempty = Threshold Nothing Nothing Nothing
    Threshold a1 b1 c1 `mappend` Threshold a2 b2 c2 = Threshold
        (lower a1 a2)
        (lower b1 b2)
        (lower c1 c2)
      where
        lower Nothing x = x
        lower x Nothing = x
        lower (Just a) (Just b) = Just (min a b)

thresholdOptions :: String -> Opt.Parser Threshold
thresholdOptions s = threshold
    <$> Opt.optional (Opt.option Opt.auto
        ( Opt.long (s++"Events")
       <> Opt.help "number of events")
        )
    <*> Opt.optional (Opt.option Buffer.kiloMega
        ( Opt.long (s++"Bytes")
       <> Opt.help "size in bytes")
        )
    <*> Opt.optional (Opt.option Opt.auto
        ( Opt.long (s++"Seconds")
       <> Opt.help "seconds")
        )

-- | Make sure that th1 (readTh) will react before th2 (appendTh), equal is OK.
isBelow :: Threshold -> Threshold -> Bool
isBelow (Threshold a1 b1 c1) (Threshold a2 b2 c2) = and [a1<!a2, b1<!b2, c1<!c2]
  where
    Just a <! Just b = a <= b
    _ <! Nothing = True
    Nothing <! _ = False

threshold :: (Maybe Int) -> (Maybe Integer) -> (Maybe Double) -> Threshold
threshold = Threshold

-- | Check if any limit has been set.
anyLimit :: Threshold -> Bool
anyLimit (Threshold a b c) = or [isJust a, isJust b, isJust c]

-- | Create new buffer.
newBuffer :: Threshold -> Threshold -> STM.STM Buffer
newBuffer appendTh readTh = Buffer
    <$> STM.newTVar mempty
    <*> pure appendTh
    <*> pure readTh

-- | Append new events to the buffer.
-- We need to respect buffer append threshold, so in case of
-- overflow, some events will not be appended (but returned).
appendBuffer :: TimeSpec -> Buffer -> [Event.Event] -> STM.STM [Event.Event]
appendBuffer ts buf evts = do
    content <- STM.readTVar $ bufData buf
    let (newContent, leftover) = appendItems content evts
    STM.writeTVar (bufData buf) newContent
    return leftover
  where
    appendItems content [] = (content, [])
    appendItems content lst@(x:xs) =
        let newContent = content `mappend` DS.singleton x
        in case thCompare ts newContent (bufAppendTh buf) of
            GT -> (content, lst)
            _ -> appendItems newContent xs

-- | Read buffer content when ready to read.
-- Read only as many events as allowed by the threshold
readBuffer :: STM.TVar TimeSpec -> Buffer -> STM.STM [Event.Event]
readBuffer tick buf = do
    ts <- STM.readTVar tick
    content <- STM.readTVar $ bufData buf
    (contentReady, leftover) <- readItems ts DS.empty content
    STM.writeTVar (bufData buf) leftover
    return $ toList contentReady
  where
    readItems ts acc content = case DS.viewr content of
        DS.EmptyR -> STM.retry
        (rest DS.:> aR) ->
            let newAcc = aR DS.<| acc
            in case thCompare ts newAcc (bufReadTh buf) of
                LT -> readItems ts newAcc rest
                _ -> return (newAcc, rest)

-- | Compare content of the buffer with the threshold
thCompare :: TimeSpec -> DS.Seq Event.Event -> Threshold -> Ordering
thCompare ts s th = maximum [thCompareLength, thCompareBytes, thCompareSeconds]
  where
    mCompare _ Nothing = LT
    mCompare actual (Just limit) = compare actual limit

    thCompareLength = mCompare (DS.length s) (thLength th)
    thCompareBytes = mCompare (foldr (+) 0 (fmap Event.sizeOf s)) (thBytes th)
    thCompareSeconds = mCompare age (thSeconds th) where
        oldest = DS.viewr s
        age = case oldest of
            (_ DS.:> aR) -> secondsSince (Event.eMonoTime aR) ts
            _ -> 0
        secondsSince :: TimeSpec -> TimeSpec -> Double
        secondsSince t1 t2 = (/ (10^(9::Int))) $ fromIntegral (t2' - t1') where
            t2' = toNanoSecs t2
            t1' = toNanoSecs t1
-}

