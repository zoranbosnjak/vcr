module Buffer where

import Control.Concurrent.STM
import Control.Monad hiding (forever)
import Data.List
import Data.Maybe
import Options.Applicative
import System.Clock

import Event

data Buffer = Buffer
    { bufData       :: TVar [Event]
    , bufCnt        :: TVar Integer
    , bufBytes      :: TVar Integer
    , bufOldest     :: TVar (Maybe TimeSpec)
    }

data Thrashold = Thrashold
    { maxEvents     :: Maybe Integer
    , maxBytes      :: Maybe Integer
    , maxSeconds    :: Maybe Double
    } deriving (Eq, Show)

anyLimit :: Thrashold -> Bool
anyLimit (Thrashold a b c) = isJust a || isJust b || isJust c

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

bufferNew :: STM Buffer
bufferNew = Buffer
    <$> newTVar []
    <*> newTVar 0
    <*> newTVar 0
    <*> newTVar Nothing

secondsSince :: TimeSpec -> TimeSpec -> Double
secondsSince t1 t2 = (/ (10^(9::Int))) $ fromIntegral (t2' - t1') where
    t2' = toNanoSecs t2
    t1' = toNanoSecs t1

bufferAppend :: Thrashold -> Buffer -> [Event] -> STM [Event]
bufferAppend th buf evts = do

    cnt <- readTVar $ bufCnt buf
    bytes <- readTVar $ bufBytes buf
    oldest <- readTVar $ bufOldest buf

    let sizes = map sizeOf evts
        times = map eMonotonicTime evts

        counts  = [(cnt+1)..]
        mem     = drop 1 $ scanl' (+) bytes sizes
        ages    = case oldest of
            Nothing -> repeat Nothing
            Just t0 -> map (Just . secondsSince t0) times

        reached :: [Bool]
        reached = zipWith3 (thOverflow th) counts mem ages
        canTake = length $ takeWhile not reached
        (a,b) = splitAt canTake evts

    -- append events
    content <- readTVar $ bufData buf
    let newContent = concat [a,content]

    writeTVar (bufData buf) newContent
    modifyTVar (bufCnt buf) (+ (fromIntegral $ length a))
    modifyTVar (bufBytes buf) (+ (foldl' (+) 0 (map sizeOf a)))
    when (isNothing oldest) $ do
        case null newContent of
            True -> return ()
            False -> do
                let x = last newContent
                writeTVar (bufOldest buf) (Just $ eMonotonicTime x)

    -- return not appended events
    return b

-- | Read buffer content when ready to read.
bufferRead :: Thrashold -> Buffer -> TVar TimeSpec -> STM [Event]
bufferRead th buf tick = do
    cnt <- readTVar $ bufCnt buf
    bytes <- readTVar $ bufBytes buf
    oldest <- readTVar $ bufOldest buf
    t <- readTVar tick
    let age = fmap (\o -> secondsSince o t) oldest

    case thOverflow th cnt bytes age of
        False -> retry
        True -> do
            writeTVar (bufCnt buf) 0
            writeTVar (bufBytes buf) 0
            writeTVar (bufOldest buf) Nothing
            swapTVar (bufData buf) []

thOverflow :: Thrashold -> Integer -> Integer -> Maybe Double -> Bool
thOverflow th cnt bytes age = or [chkSize, chkBytes, chkTime]
  where
    chkSize = cmp (maxEvents th) cnt
    chkBytes = cmp (maxBytes th) bytes
    chkTime = case age of
        Nothing -> False
        Just age' -> cmp (maxSeconds th) age'

    cmp Nothing _ = False
    cmp (Just x) y = y >= x

