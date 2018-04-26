------------------
-- |
-- Module: Event
--
-- Event description and functions.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Event
{-
( Event(..)
, Channel, channelOptions
, SourceId, sourceId, sourceIdOptions
, SessionId, sessionId
, sequenceNum
, nextSequenceNum
, now
) -} where

import qualified Data.Aeson
import           Data.Aeson.Types (typeMismatch, object, (.=), (.:))
import qualified Data.Serialize as Bin
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import           Database.HDBC
import           Data.Convertible
import qualified Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Options.Applicative as Opt
import qualified System.Clock
import           Test.QuickCheck (Arbitrary, arbitrary, getPositive, choose)

-- local imports
import qualified Encodings as Enc

data Event = Event
    { eChannel  :: Channel              -- name of the channel
    , eSourceId :: SourceId             -- id of the recorder
    , eUtcTime  :: UtcTime              -- capture utc time
    , eMonoTime :: MonoTime             -- capture monotonic (boot) time
    , eSessionId :: SessionId           -- monotonic time is valid only within
                                        -- the same session ID
    , eTrackId  :: TrackId              -- unique value for each channel startup
    , eSequence :: SequenceNum          -- incrementing sequence number
    , eValue    :: BS.ByteString        -- the event value
    } deriving (Generic, Eq, Show, Read)

instance Bin.Serialize Event

-- | One possible way to order events (by distinct key).
instance Ord Event where
    compare a b =
        compare (eChannel a) (eChannel b)
        `mappend` compare (eSessionId a) (eSessionId b)
        `mappend` compare (eMonoTime a) (eMonoTime b)

instance Arbitrary Event where
    arbitrary = Event
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (BS.pack <$> arbitrary)

instance Data.Aeson.ToJSON Event where
    toJSON
        (Event ch src (UtcTime utcTime) (MonoTime monoTime) ses trk seqNum val)
      = object
        [ "channel"     .= ch
        , "recorder"    .= src
        , "utcTime"     .= utcTime
        , "monoTime"    .= System.Clock.toNanoSecs monoTime
        , "session"     .= ses
        , "track"       .= trk
        , "sequence"    .= seqNum
        , "data"        .= Enc.hexlify val
        ]

instance Data.Aeson.FromJSON Event where
    parseJSON (Data.Aeson.Object v) = Event
        <$> v .: "channel"
        <*> v .: "recorder"
        <*> fmap UtcTime (v .: "utcTime")
        <*> fmap (MonoTime . System.Clock.fromNanoSecs) (v .: "monoTime")
        <*> v .: "session"
        <*> v .: "track"
        <*> fmap SequenceNum (v .: "sequence")
        <*> readStr (v .: "data")
      where
        readStr px = do
            s <- px
            maybe (fail "unable to parse") pure (Enc.unhexlify s)

    parseJSON invalid    = typeMismatch "Event" invalid

newtype Channel = Channel Text deriving (Generic, Eq, Ord, Show, Read)
instance Bin.Serialize Channel where
    put (Channel c) = Bin.put $ TE.encodeUtf8 c
    get = Channel . TE.decodeUtf8 <$> Bin.get
instance Data.Aeson.ToJSON Channel
instance Data.Aeson.FromJSON Channel
instance Data.Aeson.ToJSONKey Channel
instance Data.Aeson.FromJSONKey Channel
instance Arbitrary Channel where
    arbitrary = Channel . T.pack <$> arbitrary
instance Convertible Channel SqlValue where
    safeConvert (Channel val) = safeConvert val
instance Convertible SqlValue Channel where
    safeConvert val = Channel <$> safeConvert val

channelOptions :: Opt.Parser Channel
channelOptions = Channel <$> Opt.strOption
    ( Opt.long "channel"
   <> Opt.metavar "CH"
   <> Opt.help "Channel identifier"
    )

newtype SourceId = SourceId Text deriving (Generic, Eq, Show, Read)
instance Bin.Serialize SourceId where
    put (SourceId c) = Bin.put $ TE.encodeUtf8 c
    get = SourceId . TE.decodeUtf8 <$> Bin.get
instance Data.Aeson.ToJSON SourceId
instance Data.Aeson.FromJSON SourceId
instance Arbitrary SourceId where
    arbitrary = SourceId . T.pack <$> arbitrary
instance Convertible SourceId SqlValue where
    safeConvert (SourceId val) = safeConvert val
instance Convertible SqlValue SourceId where
    safeConvert val = SourceId <$> safeConvert val

sourceId :: Text -> SourceId
sourceId = SourceId

sourceIdOptions :: Opt.Parser SourceId
sourceIdOptions = SourceId <$> Opt.strOption
    ( Opt.long "ident"
   <> Opt.metavar "IDENT"
   <> Opt.help "Recorder identifier"
    )

newtype UtcTime = UtcTime Data.Time.UTCTime
    deriving (Generic, Eq, Ord, Show, Read)

instance Arbitrary UtcTime where
    arbitrary = do
        a <- day
        b <- diffT
        return $ UtcTime $ Data.Time.UTCTime a b
      where
        day = Data.Time.fromGregorian
            <$> fmap getPositive arbitrary
            <*> choose (1,12)
            <*> choose (1,31)
        diffT = Data.Time.picosecondsToDiffTime
            <$> choose (0, (24*3600*(10^(12::Int))-1))

instance Convertible UtcTime SqlValue where
    safeConvert (UtcTime val) = Right $ SqlUTCTime val

instance Convertible SqlValue UtcTime where
    safeConvert val = UtcTime <$> safeConvert val

newtype MonoTime = MonoTime System.Clock.TimeSpec
    deriving (Generic, Eq, Ord, Show, Read)

instance Arbitrary MonoTime where
    arbitrary = do
        a <- fmap getPositive arbitrary
        b <- choose (0, 999999999)
        return $ MonoTime $ System.Clock.TimeSpec a b

instance Convertible MonoTime SqlValue where
    safeConvert (MonoTime x) = Right $ SqlInteger $ System.Clock.toNanoSecs x
instance Convertible SqlValue MonoTime where
    safeConvert val = (MonoTime . System.Clock.fromNanoSecs) <$> safeConvert val

instance Bin.Serialize UtcTime where
    put (UtcTime t) = do
        Bin.put $ Data.Time.toGregorian $ Data.Time.utctDay t
        Bin.put $ Data.Time.diffTimeToPicoseconds $ Data.Time.utctDayTime t

    get = do
        (a,b,c) <- Bin.get
        ps <- Bin.get
        return $ UtcTime $ Data.Time.UTCTime
            (Data.Time.fromGregorian a b c) (Data.Time.picosecondsToDiffTime ps)

instance Bin.Serialize MonoTime where
    put (MonoTime t) = do
        Bin.put $ System.Clock.sec t
        Bin.put $ System.Clock.nsec t

    get = do
        a <- Bin.get
        b <- Bin.get
        return $ MonoTime $ System.Clock.TimeSpec a b

newtype SessionId = SessionId String deriving (Generic, Eq, Ord, Show, Read)
instance Bin.Serialize SessionId
instance Data.Aeson.ToJSON SessionId
instance Data.Aeson.FromJSON SessionId
instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary

instance Convertible SessionId SqlValue where
    safeConvert (SessionId val) = Right $ SqlString val
instance Convertible SqlValue SessionId where
    safeConvert val = SessionId <$> safeConvert val

sessionId :: String -> SessionId
sessionId = SessionId

newtype TrackId = TrackId String deriving (Generic, Eq, Ord, Show, Read)
instance Bin.Serialize TrackId
instance Data.Aeson.ToJSON TrackId
instance Data.Aeson.FromJSON TrackId
instance Arbitrary TrackId where
    arbitrary = TrackId <$> arbitrary

instance Convertible TrackId SqlValue where
    safeConvert (TrackId val) = Right $ SqlString val
instance Convertible SqlValue TrackId where
    safeConvert val = TrackId <$> safeConvert val

trackId :: String -> TrackId
trackId = TrackId

-- | Sequence number that wraps around.
newtype SequenceNum = SequenceNum Integer
    deriving (Generic, Eq, Show, Read)
instance Bin.Serialize SequenceNum
instance Data.Aeson.ToJSON SequenceNum
instance Data.Aeson.FromJSON SequenceNum

instance Bounded SequenceNum where
    minBound = SequenceNum 0
    maxBound = SequenceNum (0x10000-1)

-- | Smart constructor for SequenceNum
sequenceNum :: (Integral i) => i -> SequenceNum
sequenceNum i
    | i < fromIntegral a = error "value too small"
    | i > fromIntegral b = error "value too large"
    | otherwise = SequenceNum $ fromIntegral i
  where
    SequenceNum a = minBound
    SequenceNum b = maxBound

instance Arbitrary SequenceNum where
    arbitrary = sequenceNum <$> choose (a,b) where
        SequenceNum a = minBound
        SequenceNum b = maxBound

instance Convertible SequenceNum SqlValue where
    safeConvert (SequenceNum val) = Right $ SqlInteger val

instance Convertible SqlValue SequenceNum where
    safeConvert val = SequenceNum <$> safeConvert val

-- | Increment sequence number, respect maxBound.
nextSequenceNum :: SequenceNum -> SequenceNum
nextSequenceNum (SequenceNum sn)
    | sn == b = minBound
    | otherwise = sequenceNum $ succ sn
  where
    SequenceNum b = maxBound

-- | Get current time (UTC,boot).
now :: IO (UtcTime, MonoTime)
now = (,)
    <$> (UtcTime <$> Data.Time.getCurrentTime)
    <*> (MonoTime <$> System.Clock.getTime System.Clock.Boottime)

-- | Convert monotonic time to seconds.
monoTimeToSeconds :: MonoTime -> Double
monoTimeToSeconds (MonoTime t) =
    (/ (10^(9::Int))) $ fromInteger $ System.Clock.toNanoSecs t

