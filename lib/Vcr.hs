
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Vcr where

import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.Text as Text

import           Test.QuickCheck

-- local imports
import           Time
import           Sequential

type Channel = Text
type SessionId = Text
type TrackId = Text
type SequenceNumber = Periodic 0x100000000

data Event a = Event
    { eChannel      :: Channel          -- channel name
    , eTimeMono     :: MonoTimeNs       -- monotonically incrementing time
    , eTimeUtc      :: UtcTime          -- utc time
    , eSessionId    :: SessionId        -- unique session identifier
    , eTrackId      :: TrackId          -- unique track identifier
    , eSequence     :: SequenceNumber   -- sequence number
    , eValue        :: a                -- value
    } deriving (Generic, Eq, Show)

instance Functor Event where
    fmap f event = event { eValue = f (eValue event) }

instance (ToJSON a) => ToJSON (Event a) where
    toJSON (Event ch tMono tUtc sid tid tSeq val) = object
        [ "channel" .= ch
        , "tMono"   .= tMono
        , "tUtc"    .= tUtc
        , "session" .= sid
        , "track"   .= tid
        , "sequence" .= tSeq
        , "value"   .= val
        ]

instance (FromJSON a) => FromJSON (Event a) where
    parseJSON = withObject "Event" $ \v -> Event
        <$> v .: "channel"
        <*> v .: "tMono"
        <*> v .: "tUtc"
        <*> v .: "session"
        <*> v .: "track"
        <*> v .: "sequence"
        <*> v .: "value"

instance (Arbitrary a) => Arbitrary (Event a) where
    arbitrary = Event
        <$> fmap Text.pack arbitrary
        <*> arbitrary
        <*> arbitraryUtc
        <*> fmap Text.pack arbitrary
        <*> fmap Text.pack arbitrary
        <*> arbitrary
        <*> arbitrary

-- | Add delta-t (nanoseconds) to event.
nextEvents :: MonoTimeNs -> Event a -> [Event a]
nextEvents dt e0 = e:nextEvents dt e where
    e = e0
        { eTimeMono = eTimeMono e + dt
        , eTimeUtc = addUtcTime dt (eTimeUtc e)
        }

