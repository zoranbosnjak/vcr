{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common VCR data types and helper functions.

module Vcr where

import           GHC.Generics (Generic)
import           Data.Text as Text
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AesonP
import qualified Data.List.NonEmpty as NE
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe
import           Pipes
import qualified Pipes.Prelude as PP
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
    = PlayError String
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

-- | Encode (pretty) to JSON.
encodeJSONPrettyL :: (Data.Aeson.ToJSON a) => a -> BSL.ByteString
encodeJSONPrettyL = AesonP.encodePretty'
    AesonP.defConfig {AesonP.confCompare = compare}

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

type LogAction m = Text -> m ()
type Recorder m a r = LogAction m -> Consumer a m r

-- | Create JSON based recorder from ByteString recorder.
jsonRecorder :: (ToJSON a, Functor m) => Recorder m BS.ByteString r -> Recorder m a r
jsonRecorder rec = \logM -> PP.map encodeJSON >-> rec logM

-- | Player direction.
data Direction = Forward | Backward
    deriving (Generic, Eq, Enum, Bounded, Show)

-- | Available text based filters.
data TextFilter
    = TextFilterMatch Text      -- match exactly
    | TextFilterIsPrefix Text   -- starts with given text
    | TextFilterIsSuffix Text   -- ends with given text
    | TextFilterIsInfix Text    -- contains given text
    deriving (Generic, Eq, Show, ToJSON, FromJSON)

applyTextFilter :: TextFilter -> Text -> Bool
applyTextFilter = \case
    TextFilterMatch t -> (==) t
    TextFilterIsPrefix t -> Text.isPrefixOf t
    TextFilterIsSuffix t -> Text.isSuffixOf t
    TextFilterIsInfix t -> Text.isInfixOf t

-- | Event filters.
-- We want predicates of type (Event a -> Bool),
-- but this is in general not serializable (over http).
-- So, instead of a function, use this (serializable) data structure.
data Filter
    = FChannel TextFilter
    | FSession TextFilter
    | FTrack TextFilter
    | And Filter Filter
    | Or Filter Filter
    | Not Filter
    | Pass
    deriving (Generic, Eq, Show, ToJSON, FromJSON)

applyFilter :: Filter -> Event a -> Bool
applyFilter flt event = case flt of
    FChannel tf -> applyTextFilter tf (eChannel event)
    FSession tf -> applyTextFilter tf (eSessionId event)
    FTrack tf   -> applyTextFilter tf (eTrackId event)
    And f1 f2 -> applyFilter f1 event && applyFilter f2 event
    Or f1 f2 -> applyFilter f1 event || applyFilter f2 event
    Not f -> not $ applyFilter f event
    Pass -> True

-- | Helper function to construct channel filter.
onlyChannels :: Foldable t => t Channel -> Filter
onlyChannels = Prelude.foldr (Or . FChannel . TextFilterMatch) (Not Pass)

-- | Player methods structure.
data Player m ix a = Player
    { limits    :: m (ix, ix)       -- (first, last) index of a player
    , middle    :: ix -> ix -> m ix -- get valid index near the middle of the interval
    , peekItem  :: ix -> m a        -- peek item at given index
    , runPlayer :: Direction -> ix
        -- When a filter is active, there might also
        -- be a timeout interval. This makes a producer
        -- 'leaky', to yield some (otherwise filtered out) event,
        -- so that consumer can progress.
        -> Maybe (Filter, Maybe NominalDiffTime)
        -> Producer (ix, a) m ()
    }

-- | Dummy player.
mkDummyPlayer :: MonadThrow m => Player m ix a
mkDummyPlayer = Player
    { limits = throwM $ PlayError "dummy player"
    , middle = \_ix1 _ix2 -> throwM $ PlayError "dummy player"
    , peekItem = \_ix -> throwM $ PlayError "dummy player"
    , runPlayer = \_direction _ix _flt -> return ()
    }

-- | Make one step from given index and return next (ix, a).
nextItem :: MonadThrow m => Player m ix a -> ix -> Direction -> m (ix, a)
nextItem player ix direction = do
    result <- next (runPlayer player direction ix Nothing >-> PP.drop 1)
    case result of
        Left e -> throwM $ PlayError $ show e
        Right (val, _producer') -> return val

-- | 'Player m ix' is a functor.
instance Monad m => Functor (Player m ix) where
    fmap f player = Player
        { limits = limits player
        , middle = middle player
        , peekItem = \ix -> f <$> peekItem player ix
        , runPlayer = \direction ix flt -> do
            runPlayer player direction ix flt >-> PP.mapM (\(a,b) -> do
                return (a, f b))
        }

-- | Common index data type for all players
-- Correct 'Ord' instance is important for 'bisect' function.
data Index
    = Index (NE.NonEmpty Integer)
    deriving (Generic, Eq, Ord, ToJSON, FromJSON)

instance Show Index where
    show (Index lst) = show $ NE.toList lst

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
    , runPlayer = \direction ix flt -> do
        let ix' = fromIndex ix
        runPlayer player direction ix' flt >-> PP.mapM (\(a,b) -> do
            return (toIndex a, b))
    }

-- | Filter events.
-- When timeout expires, allow first event to pass through,
-- regardless of the filter.
dashEvents :: Functor m => Direction -> (Filter, Maybe NominalDiffTime)
    -> Pipe (ix, Event a) (ix, Event a) m r
dashEvents _direction (flt, Nothing) = forever $ do
    (ix, event) <- await
    when (applyFilter flt event) $ do
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
        case applyFilter flt event' of
            False -> do
                let dt = diffUTCTime (eTimeUtc event') (eTimeUtc event)
                case dt `compareOperator` timeout of
                    False -> loop event
                    True -> do
                        yield (ix, event')
                        loop event'
            True -> do
                yield (ix, event')
                loop event'

-- | Create channel filter pipe.
dashEvents' :: Functor m => Direction -> Maybe (Filter, Maybe NominalDiffTime)
    -> Pipe (ix, Event a) (ix, Event a) m r
dashEvents' _direction Nothing = cat
dashEvents' direction (Just (flt, timeout)) =
    dashEvents direction (flt, timeout)

-- | Find (ix, item) where item condition changes from '<', to '>='.
bisect :: (Ord ix, Monad m) =>
    Player m ix a -> (a -> Ordering) -> m (Maybe (ix, a))
bisect player checkItem = do
    (i1, i2) <- limits player
    runMaybeT $ go i1 i2
  where
    probe i = do
        a <- peekItem player i
        return (a, checkItem a)
    go i1 i2 = do
        guard (i2 >= i1)
        (a1, x1) <- lift $ probe i1
        case x1 == LT of
            False -> return (i1, a1)   -- found it
            True -> do
                guard (x1 == LT)
                i <- lift $ middle player i1 i2
                case i == i1 of
                    False -> do     -- big interval
                        (a, x) <- lift $ probe i
                        case x of
                            LT -> go i i2
                            EQ -> return (i, a)
                            GT -> go i1 i
                    True -> do      -- no more items between i1 and i2
                        (a, x2) <- lift $ probe i2
                        guard (x2 /= LT)
                        return (i2, a)

-- | Find UTC time.
findEventByTimeUtc :: (Ord ix, Monad m) =>
    Player m ix (Event a) -> UtcTime -> m (Maybe (ix, Event a))
findEventByTimeUtc s t = bisect s $ \evt -> compare (eTimeUtc evt) t

