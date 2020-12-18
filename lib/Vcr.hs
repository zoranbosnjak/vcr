{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Vcr where

import           GHC.Generics (Generic)
import           Data.Kind (Type)
import           Data.Text as Text
import           Data.Aeson
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Pipes
import qualified Pipes.Prelude as PP

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

-- | Event generator (for testing purposes).
eventGen :: Monad m => (a -> m a) -> a -> Producer a m b
eventGen f a = do
    yield a
    a' <- lift $ f a
    eventGen f a'

-- | Add delta-t (nanoseconds) to each event, for testing purposes.
eventGenAddTime :: Monad m => MonoTimeNs -> Event a -> Producer (Event a) m b
eventGenAddTime dt = eventGen f where
    f e = return $ e
        { eTimeMono = eTimeMono e + dt
        , eTimeUtc = addMonoTimeNS dt (eTimeUtc e)
        }

-- | Streaming components

-- | Recorders (Item consumers)

class Monad m => IsRecorder s m where
    type RecItem s :: Type

    mkRecorder :: s
        -> (Text -> m ()) -- log message
        -> Consumer (RecItem s) m r

-- | Players (Item producers)

class (Monad m, Ord (Index s)) => Indexed s m where
    type Index s :: Type

    -- | Return (first, overflow) index, Nothing if empty.
    limits :: s -> m (Maybe (Index s, Index s))

    -- | Calculate valid index near the middle of the interval.
    middle :: s -> Index s -> Index s -> m (Index s)

class Indexed s m => IsPlayer s m where
    type RepItem s :: Type

    -- Peek item from given index.
    peekItem :: s -> Index s -> m (RepItem s)

-- | List is a player.
instance Monad m => Indexed [a] m where
    type Index [a] = Int

    limits [] = return Nothing
    limits lst = return $ Just (0, Prelude.length lst)
    middle _lst a b = return ((a + b) `div` 2)

instance Monad m => IsPlayer [a] m where
    type RepItem [a] = a

    peekItem lst ix = return (lst !! ix)

-- | Player direction.
data Direction = Backward | Forward
    deriving (Generic, Eq, Show)

instance Arbitrary Direction where
    arbitrary = elements [Backward, Forward]

-- | Faster player version (without channel filter).
class IsPlayer s m => IsRawPlayer s m where
    mkRawPlayer ::
        s
        -> Direction
        -> Index s
        -> Producer (Index s, RepItem s) m (Index s)

-- | List is a RawPlayer.
instance Monad m => IsRawPlayer [a] m where
    mkRawPlayer lst direction ix = case direction of
        Backward -> do
            each (Prelude.reverse $ Prelude.take ix lst')
            return 0
        Forward -> do
            each (Prelude.drop ix lst')
            return (Prelude.length lst)
      where
        lst' = Prelude.zip [0..] lst

-- | Source of events (with channel filter support).
class IsPlayer s m => IsEventPlayer s m where
    mkEventPlayer ::
        s
        -> Direction
        -> Index s
        -> Maybe [Channel]
        -> Producer (Index s, Either (Event ()) (RepItem s)) m (Index s)

-- | If the 'Item s' is 'Event a', we can make it an IsEventPlayer instance.
-- It's OVERLAPPABLE, unly use it if no other (more specific instance) instance is provided.
instance {-# OVERLAPPABLE #-} (IsPlayer s m, IsRawPlayer s m, RepItem s ~ Event a) => IsEventPlayer s m where
    mkEventPlayer s direction ix mFilter =
        mkRawPlayer s direction ix >-> PP.map (\(i, a) -> (i, f a))
      where
        f evt = case mFilter of
            Nothing -> Right evt
            Just channels -> case eChannel evt `elem` channels of
                False -> Left (fmap (const ()) evt)
                True -> Right evt

-- | Find index where item condition changes from '<', to '>='.
bisect :: IsPlayer s m => s -> (RepItem s -> Ordering) -> m (Maybe (Index s))
bisect s checkItem = limits s >>= \case
    Nothing -> return Nothing
    Just (i1, i2) -> runMaybeT $ go i2 i1 i2
  where
    probe i = checkItem <$> peekItem s i
    go iOut i1 i2 = do
        guard (i2 > i1)
        x1 <- lift $ probe i1
        guard (x1 /= GT)
        case x1 == EQ of
            True -> return i1   -- exact match
            False -> do
                guard (x1 == LT)
                i <- lift $ middle s i1 i2
                case i == i1 of
                    False -> do     -- big interval
                        x <- lift $ probe i
                        case x of
                            LT -> go iOut i i2
                            EQ -> return i
                            GT -> go iOut i1 i
                    True -> do      -- no more items between i1 and i2
                        guard (i2 /= iOut)
                        x2 <- lift $ probe i2
                        guard (x2 /= LT)
                        return i2

-- | Find UTC time.
findEventByTimeUtc :: (IsPlayer s m, RepItem s ~ Event a)
    => s -> UtcTime -> m (Maybe (Index s))
findEventByTimeUtc s t = bisect s $ \evt -> compare (eTimeUtc evt) t

