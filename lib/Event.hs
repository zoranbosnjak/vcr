
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Event where

import           GHC.Generics (Generic)
import           Data.Text as Text
import           Data.Aeson

-- local imports
import           Time
import           Sequential

type Channel = Text
type SessionId = Text
type TrackId = Text
type SequenceNumber = Periodic 0x100000000

data Event a = Event
    { eChannel :: Channel       -- channel name
    , eTimeMono :: MonoTime     -- monotonically incrementing time
    , eTimeWall :: UtcTime      -- wall clock time
    , eSessionId :: SessionId   -- unique session identifier
    , eTrackId  :: TrackId      -- unique track identifier
    , eSequence :: SequenceNumber -- sequence number
    , eValue    :: a            -- value
    } deriving (Generic, Eq, Show)

instance Functor Event where
    fmap f event = event { eValue = f (eValue event) }

instance (ToJSON a) => ToJSON (Event a) where
    toJSON (Event ch tMono tWall sid tid tSeq val) = object
        [ "channel" .= ch
        , "tMono"   .= tMono
        , "tWall"   .= tWall
        , "session" .= sid
        , "track"   .= tid
        , "sequence" .= tSeq
        , "value"   .= val
        ]

instance (FromJSON a) => FromJSON (Event a) where
    parseJSON = withObject "Event" $ \v -> Event
        <$> v .: "channel"
        <*> v .: "tMono"
        <*> v .: "tWall"
        <*> v .: "session"
        <*> v .: "track"
        <*> v .: "sequence"
        <*> v .: "value"

