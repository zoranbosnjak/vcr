----------------
-- |
-- Module:      Buffer
--
-- Buffer manipulations
--

module Buffer
( Buffer
, Thrashold (thLength, thBytes, thSeconds)
, thrashold, thrasholdOptions
, isBelow

-- buffer manipulation
, newBuffer
, appendBuffer
, readBuffer

-- helpers
, kiloMega
, anyLimit
) where

import qualified Control.Concurrent.STM as STM
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.Sequence as DS
import qualified Options.Applicative as Opt
import System.Clock (TimeSpec, toNanoSecs)

import qualified Event

data Buffer = Buffer
    { bufData       :: STM.TVar (DS.Seq Event.Event)
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

thrasholdOptions :: String -> Opt.Parser Thrashold
thrasholdOptions s = thrashold
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
kiloMega :: Opt.ReadM Integer
kiloMega = Opt.eitherReader $ \arg -> do
    let (a,b) = case last arg of
            'k' -> (init arg, 1)
            'M' -> (init arg, 2)
            'G' -> (init arg, 3)
            _ -> (arg, 0)
    case reads a of
        [(r, "")] -> return (r * (1024^(b::Int)))
        _         -> Left $ "cannot parse value `" ++ arg ++ "'"

-- | Create new buffer.
newBuffer :: Thrashold -> Thrashold -> STM.STM Buffer
newBuffer appendTh readTh = Buffer
    <$> STM.newTVar mempty
    <*> pure appendTh
    <*> pure readTh

-- | Append new events to the buffer.
-- We need to respect buffer append thrashold, so in case of
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
-- Read only as many events as allowed by the thrashold
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

-- | Compare content of the buffer with the thrashold
thCompare :: TimeSpec -> DS.Seq Event.Event -> Thrashold -> Ordering
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

