{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Vcr where

import           GHC.Generics (Generic)
import           Data.Kind (Type)
import           Data.Text as Text
import           Data.Aeson
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Pipes
import qualified Pipes.Prelude as PP
import qualified Text.Regex.TDFA as Reg
import qualified Text.Regex.TDFA.Text as Reg
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           Test.QuickCheck

-- local imports
import           Time
import           Sequential

type Channel = Text
type SessionId = Text
type TrackId = Text
type SequenceNumber = Periodic 0x100000000

data VcrException
    = RegexCompileError String
    | PlayError String
    | IndexError String
    | JSONDecodeError String
    deriving Show

instance Exception VcrException

-- | Encode to JSON.
encodeJSON :: ToJSON a => a -> BS.ByteString
encodeJSON = BSL.toStrict . Data.Aeson.encode

-- | Decode JSON object.
decodeJSON :: (MonadThrow m, FromJSON a) => BS.ByteString -> m a
decodeJSON s = case eitherDecodeStrict s of
    Left e -> throwM $ JSONDecodeError $ show s ++ ", error: " ++ e
    Right val -> return val

data Event a = Event
    { eChannel      :: Channel          -- channel name
    , eTimeMono     :: MonoTimeNs       -- monotonically incrementing time
    , eTimeUtc      :: UtcTime          -- utc time
    , eSessionId    :: SessionId        -- unique session identifier
    , eTrackId      :: TrackId          -- unique track identifier
    , eSequence     :: SequenceNumber   -- sequence number
    , eValue        :: a                -- value
    } deriving (Generic, Eq, Show, Functor)

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

class Monad m => Recorder s m a where
    mkRecorder :: s
        -> (Text -> m ())   -- log message function
        -> Consumer a m r

-- | Create 'Event' recorder from 'ByteString' recorder.
instance {-# OVERLAPPABLE #-} (ToJSON a, Recorder s m BS.ByteString) => Recorder s m (Event a) where
    mkRecorder s logM =
        PP.map encodeJSON
        >-> mkRecorder s logM

-- | Player direction.
data Direction = Forward | Backward
    deriving (Generic, Eq, Show)

instance Arbitrary Direction where
    arbitrary = elements [Forward, Backward]

-- | Data structures with 'Index'.
class (Monad m, Ord (Index s)) => Indexed s m where
    type Index s :: Type    -- index type

    -- | Return (first, overflow) index of the structure.
    limits :: s -> m (Index s, Index s)

    -- | Calculate valid index near the middle of the interval,
    middle :: s -> Index s -> Index s -> m (Index s)

-- | Can peek item at given index.
class Indexed s m => HasItem s m a where
    peekItem :: s -> Index s -> m a

-- | Players (Item producers)

-- | Fast player (no filter support)
class Indexed s m => Player s m a where
    -- | Create producer.
    mkPlayer ::
        s
        -> Direction
        -> Index s
        -> Producer (Index s, a) m ()

-- | Player wtih channel filter support.
class Indexed s m => PlayerF s m a where
    mkPlayerF ::
        s
        -> Direction
        -> Index s

        -- Optional timeout interval makes a producer to
        -- yield some (otherwise filtered out) event,
        -- so that consumer can progress.
        -> Maybe MonoTimeNs

        -- Regex, representing channel filter
        -- This needs to be simple type, to be able
        -- to encode this argument over http(s).
        -> Maybe Text

        -> Producer (Index s, Event a) m ()

instance Monad m => Indexed [a] m where
    type Index [a] = Int

    limits lst = return $ (0, Prelude.length lst)
    middle _lst a b = return ((a + b) `div` 2)

instance Monad m => HasItem [a] m a where
    peekItem lst ix = return (lst !! ix)

instance Monad m => Player [a] m a where
    mkPlayer lst direction ix = case direction of
        Forward -> do
            each (Prelude.drop ix lst')
        Backward -> do
            each (Prelude.reverse $ Prelude.take ix lst')
      where
        lst' = Prelude.zip [0..] lst

-- | Compile regular expression
regexCompile :: Text -> Either String Reg.Regex
regexCompile = Reg.compile Reg.defaultCompOpt (Reg.ExecOption False)

-- | Create Channel predicate out of compiled regex.
mkPredicate :: Reg.RegexLike regex source => regex -> source -> Bool
mkPredicate re ch = isJust $ Reg.matchOnce re ch

-- | Turn regex filter to a predicate function.
chPredicate :: Text -> Either String (Channel -> Bool)
chPredicate t = mkPredicate <$> regexCompile t

-- | Filter events.
-- When timeout expires, allow first event to pass through,
-- regardless of the filter.
dashEvents :: Functor m => Direction -> Maybe MonoTimeNs -> (Channel -> Bool)
    -> Pipe (ix, Event a) (ix, Event a) m r
dashEvents _direction Nothing flt = forever $ do
    (ix, event) <- await
    when (flt $ eChannel event) $ do
        yield (ix, event)
dashEvents direction (Just timeout) flt = do
    (ix, event) <- await
    yield (ix, event)
    loop event
  where
    compareOperator = case direction of
        Forward -> (>=)
        Backward -> (<=)
    loop event = do
        (ix, event') <- await
        case flt (eChannel event') of
            False -> do
                let dt = eTimeMono event' - eTimeMono event
                case dt `compareOperator` timeout of
                    False -> loop event
                    True -> do
                        yield (ix, event')
                        loop event'
            True -> do
                yield (ix, event')
                loop event'

-- | Create channel filter pipe.
dashEvents' :: MonadThrow m
    => Direction
    -> Maybe MonoTimeNs     -- timeout
    -> Maybe Text           -- ch filter regex
    -> Pipe (ix, Event a) (ix, Event a) m r
dashEvents' _direction _timeout Nothing = cat
dashEvents' direction timeout (Just flt) = either
    (throwM . RegexCompileError) (dashEvents direction timeout) (chPredicate flt)

-- | Create 'PlayerF a' from 'Player (Event a)'.
instance {-# OVERLAPPABLE #-} (MonadThrow m, Indexed s m, Player s m (Event a)) => PlayerF s m a where
    mkPlayerF s direction ix timeout flt =
        mkPlayer s direction ix
        >-> dashEvents' direction timeout flt

-- | Find (ix, item) where item condition changes from '<', to '>='.
bisect :: HasItem s m a => s -> (a -> Ordering) -> m (Maybe (Index s, a))
bisect s checkItem = do
    (i1, i2) <- limits s
    runMaybeT $ go i2 i1 i2
  where
    probe i = do
        a <- peekItem s i
        return (a, checkItem a)
    go iOut i1 i2 = do
        guard (i2 > i1)
        (a1, x1) <- lift $ probe i1
        guard (x1 /= GT)
        case x1 == EQ of
            True -> return (i1, a1)   -- exact match
            False -> do
                guard (x1 == LT)
                i <- lift $ middle s i1 i2
                case i == i1 of
                    False -> do     -- big interval
                        (a, x) <- lift $ probe i
                        case x of
                            LT -> go iOut i i2
                            EQ -> return (i, a)
                            GT -> go iOut i1 i
                    True -> do      -- no more items between i1 and i2
                        guard (i2 /= iOut)
                        (a, x2) <- lift $ probe i2
                        guard (x2 /= LT)
                        return (i2, a)

-- | Find UTC time.
findEventByTimeUtc :: forall a s m. HasItem s m (Event a) => s -> UtcTime -> m (Maybe (Index s, Event a))
findEventByTimeUtc s t = bisect s $ \evt -> compare (eTimeUtc evt) t

