-- | This module implements 'Periodic' value which can be
-- used as a 'sequence number' for the 'Event'.
-- The value wraps around to 'zero', when reaches a 'Period'.

module Sequential where

import           GHC.Generics (Generic)
import           GHC.TypeNats
import           Data.Proxy
import           Data.Bool
import           Data.Aeson

import           Test.QuickCheck

class Sequential a where
    sequenceToInteger   :: a -> Integer
    firstSequence       :: a
    nextSequence        :: a -> a
    compareSequences    :: a -> a -> Ordering

    countSequential     :: [a]
    countSequential     = go firstSequence where
        go x = x:go (nextSequence x)

-- | For test purposes only. It never wraps to zero.
instance Sequential Integer where
    sequenceToInteger   = id
    firstSequence       = 0
    nextSequence        = succ
    compareSequences    = compare
    countSequential     = [0..]

data Periodic (n :: Nat) = UnsafeMkPeriodic { getPeriodic :: !Integer}
    deriving (Generic, Eq)

instance ToJSON (Periodic n) where
    toJSON = toJSON . getPeriodic

instance FromJSON (Periodic n) where
    parseJSON val = UnsafeMkPeriodic <$> parseJSON val

instance KnownNat n => Sequential (Periodic n) where
    sequenceToInteger   = getPeriodic
    firstSequence       = UnsafeMkPeriodic 0
    nextSequence (UnsafeMkPeriodic x)
        | y <= 0 = error "underflow"
        | y > top = error "overflow"
        | y == top = UnsafeMkPeriodic 0
        | otherwise = UnsafeMkPeriodic y
      where
        y = succ x
        top = fromIntegral (natVal (Proxy @n))
    compareSequences (UnsafeMkPeriodic a) (UnsafeMkPeriodic b)
        | a == b = EQ
        | otherwise =
            let delta = (b - a) `mod` top
                half = top `div` 2
            in bool LT GT (delta < half)
      where
        top = fromIntegral (natVal (Proxy @n))

instance KnownNat n => Show (Periodic n) where
    show x = show (sequenceToInteger x) ++ "/" ++ show (top :: Integer)
      where
        top = fromIntegral (natVal (Proxy @n))

instance KnownNat n => Arbitrary (Periodic n) where
    arbitrary = UnsafeMkPeriodic <$> choose (0, pred top)
      where
        top = fromIntegral (natVal (Proxy @n))

