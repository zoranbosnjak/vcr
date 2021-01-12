{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Vcr where

import           GHC.Generics (Generic)
import           Data.Text as Text
import           Data.Aeson
import           Data.Maybe
import qualified Data.List.NonEmpty as NE
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Pipes
import qualified Pipes.Prelude as PP
import qualified Text.Regex.TDFA as Reg
import qualified Text.Regex.TDFA.Text as Reg
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

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
    | DecodeError String
    deriving Show

instance Exception VcrException

-- | Encode to JSON.
encodeJSON :: ToJSON a => a -> BS.ByteString
encodeJSON = BSL.toStrict . Data.Aeson.encode

-- | Decode JSON object.
decodeJSON :: (MonadThrow m, FromJSON a) => BS.ByteString -> m a
decodeJSON s = case eitherDecodeStrict s of
    Left e -> throwM $ DecodeError $ show s ++ ", error: " ++ e
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

-- | Streaming components

type Recorder m a r = (Text -> m ()) -> Consumer a m r

-- | Create JSON based recorder from ByteString recorder.
jsonRecorder :: (ToJSON a, Functor m) => Recorder m BS.ByteString r -> Recorder m a r
jsonRecorder rec = \logM -> PP.map encodeJSON >-> rec logM

-- | Player direction.
data Direction = Forward | Backward
    deriving (Generic, Eq, Show)

-- | Player methods structure.
data Player m ix a = Player
    { limits    :: m (ix, ix)       -- (first, overflow) index of a player
    , middle    :: ix -> ix -> m ix -- get valid index near the middle of the interval
    , peekItem  :: ix -> m a        -- peek item at given index
    , mkPlayer  :: Direction -> ix
        -- Optional regex, representing channel filter.
        -- This needs to be simple type, to be able
        -- to encode this argument over http(s).
        -- When a filter is active, there might also
        -- be a timeout interval. This makes a producer
        -- 'leaky', to yield some (otherwise filtered out) event,
        -- so that consumer can progress.
        -> Maybe (Text, Maybe MonoTimeNs)
        -> Producer (ix, a) m ()
    }

-- | 'Player m ix' is a functor.
instance Monad m => Functor (Player m ix) where
    fmap f player = Player
        { limits = limits player
        , middle = middle player
        , peekItem = \ix -> f <$> peekItem player ix
        , mkPlayer = \direction ix flt -> do
            mkPlayer player direction ix flt >-> PP.mapM (\(a,b) -> do
                return (a, f b))
        }

-- | Common index data type for all players
-- Correct 'Ord' instance is important for 'bisect' function.
data Index
    = Index (NE.NonEmpty Integer)
    deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

-- | Index convesion functions.
-- It is important to preserve ordering, that is:
-- compare ix1 ix2 == compare (toIndex ix1) (toIndex ix2)
-- compare ix1 ix2 == compare (fromIndex ix1) (fromIndex ix2)
class IsIndex ix where
    toIndex   :: ix -> Index
    fromIndex :: Index -> ix

instance IsIndex Index where
    toIndex = id
    fromIndex = id

instance {-# OVERLAPPABLE #-} Integral a => IsIndex a where
    toIndex val = Index $ (toInteger val) NE.:| []
    fromIndex (Index lst) = fromInteger $ NE.head lst

-- | Change player's index type.
reindex :: (IsIndex ix, Monad m) => Player m ix a -> Player m Index a
reindex player = Player
    { limits = do
        (ix1, ix2) <- limits player
        return (toIndex ix1, toIndex ix2)
    , middle = \ix1 ix2 -> do
        ix <- middle player (fromIndex ix1) (fromIndex ix2)
        return $ toIndex ix
    , peekItem = \ix -> do
        peekItem player (fromIndex ix)
    , mkPlayer = \direction ix flt -> do
        let ix' = fromIndex ix
        mkPlayer player direction ix' flt >-> PP.mapM (\(a,b) -> do
            return (toIndex a, b))
    }

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
dashEvents :: Functor m => Direction -> ((Channel -> Bool), Maybe MonoTimeNs)
    -> Pipe (ix, Event a) (ix, Event a) m r
dashEvents _direction (flt, Nothing) = forever $ do
    (ix, event) <- await
    when (flt $ eChannel event) $ do
        yield (ix, event)
dashEvents direction (flt, Just timeout) = do
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
    -> Maybe (Text, Maybe MonoTimeNs)
    -> Pipe (ix, Event a) (ix, Event a) m r
dashEvents' _direction Nothing = cat
dashEvents' direction (Just (flt, timeout)) = case chPredicate flt of
    Left e -> throwM $ RegexCompileError e
    Right predicate -> dashEvents direction (predicate, timeout)

-- | Find (ix, item) where item condition changes from '<', to '>='.
bisect :: (Ord ix, Monad m) =>
    Player m ix a -> (a -> Ordering) -> m (Maybe (ix, a))
bisect player checkItem = do
    (i1, i2) <- limits player
    runMaybeT $ go i2 i1 i2
  where
    probe i = do
        a <- peekItem player i
        return (a, checkItem a)
    go iOut i1 i2 = do
        guard (i2 > i1)
        (a1, x1) <- lift $ probe i1
        guard (x1 /= GT)
        case x1 == EQ of
            True -> return (i1, a1)   -- exact match
            False -> do
                guard (x1 == LT)
                i <- lift $ middle player i1 i2
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
findEventByTimeUtc :: (Ord ix, Monad m) =>
    Player m ix (Event a) -> UtcTime -> m (Maybe (ix, Event a))
findEventByTimeUtc s t = bisect s $ \evt -> compare (eTimeUtc evt) t

