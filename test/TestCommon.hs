{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test structures and helper functions.

module TestCommon where

import qualified Data.Text             as T
import           Data.Time
import           Pipes
import           Test.Tasty.QuickCheck as QC

import           Sequential
import           Time
import           Vcr

instance Arbitrary UTCTime where
    arbitrary = arbitraryUtc

instance Arbitrary T.Text where
    arbitrary = fmap T.pack arbitrary

instance (Arbitrary a) => Arbitrary (Event a) where
    arbitrary = Event
        <$> arbitrary
        <*> arbitrary
        <*> arbitraryUtc
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Direction where
    arbitrary = elements [Forward, Backward]

-- | Event stream generation helper function.
nextEvent :: Monad m => m Channel -> m MonoTimeNs -> Event a -> m (Event a)
nextEvent getChannel getDt event = do
    ch <- getChannel
    dt <- getDt
    pure $ event
        { eChannel = ch
        , eTimeMono = eTimeMono event + dt
        , eTimeUtc = addMonoTimeNS dt (eTimeUtc event)
        , eSequence = nextSequence (eSequence event)
        }

-- | Random event generator (Gen monad).
genEvents :: Arbitrary a => [Channel] -> MonoTimeNs -> UtcTime -> Gen [Event a]
genEvents channels dt t0 = do
    event <- arbitrary
    ch <- elements channels
    go $ event
        { eChannel = ch
        , eTimeUtc = t0
        }
  where
    go :: Event a -> Gen [Event a]
    go event = (:) <$> pure event <*> do
        event' <- nextEvent (elements channels) (choose (dt, 1000*dt)) event
        go event'

-- | Event generator as producer.
produceEvents :: (MonadIO m, Arbitrary a) => [Channel] -> MonoTimeNs -> UtcTime
    -> Producer (Event a) m r
produceEvents channels dt t0 = do
    event <- liftIO $ generate arbitrary
    ch <- liftIO $ generate $ elements channels
    go $ event
        { eChannel = ch
        , eTimeUtc = t0
        }
  where
    go event = do
        yield event
        event' <- nextEvent
            (liftIO $ generate $ elements channels)
            (liftIO $ generate $ choose (dt, 1000*dt))
            event
        go event'
