------------------
-- |
-- Module: Event
--
-- Event description and functions.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Time
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
    , eSequence :: SequenceNum          -- incrementing sequence number
    , eValue    :: BS.ByteString        -- the event value
    } deriving (Generic, Eq, Show, Read)

instance Bin.Serialize Event

instance Arbitrary Event where
    arbitrary = Event
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (BS.pack <$> arbitrary)

instance Data.Aeson.ToJSON Event where
    toJSON (Event ch src (UtcTime utcTime) (MonoTime monoTime) ses seqNum val)
      = object
        [ "channel"     .= ch
        , "recorder"    .= src
        , "utcTime"     .= utcTime
        , "monoTime"    .= System.Clock.toNanoSecs monoTime
        , "session"     .= ses
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
        <*> v .: "sequence"
        <*> readStr (v .: "data")
      where
        readStr px = do
            s <- px
            maybe (fail "unable to parse") pure (Enc.unhexlify s)

    parseJSON invalid    = typeMismatch "Event" invalid

newtype Channel = Channel String deriving (Generic, Eq, Show, Read)
instance Bin.Serialize Channel
instance Data.Aeson.ToJSON Channel
instance Data.Aeson.FromJSON Channel
instance Arbitrary Channel where
    arbitrary = Channel <$> arbitrary

channelOptions :: Opt.Parser Channel
channelOptions = Channel <$> Opt.strOption
    ( Opt.long "channel"
   <> Opt.metavar "CH"
   <> Opt.help "Channel identifier"
    )

newtype SourceId = SourceId String deriving (Generic, Eq, Show, Read)
instance Bin.Serialize SourceId
instance Data.Aeson.ToJSON SourceId
instance Data.Aeson.FromJSON SourceId
instance Arbitrary SourceId where
    arbitrary = SourceId <$> arbitrary

sourceId :: String -> SourceId
sourceId = SourceId

sourceIdOptions :: Opt.Parser SourceId
sourceIdOptions = SourceId <$> Opt.strOption
    ( Opt.long "ident"
   <> Opt.metavar "IDENT"
   <> Opt.help "Recorder identifier"
    )

newtype UtcTime = UtcTime Data.Time.UTCTime
    deriving (Generic, Eq, Show, Read, Ord)

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

newtype MonoTime = MonoTime System.Clock.TimeSpec
    deriving (Generic, Eq, Show, Read)

instance Arbitrary MonoTime where
    arbitrary = do
        a <- fmap getPositive arbitrary
        b <- choose (0, 999999999)
        return $ MonoTime $ System.Clock.TimeSpec a b

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

-- | Sequence number that wraps around.

newtype SessionId = SessionId String deriving (Generic, Eq, Show, Read)
instance Bin.Serialize SessionId
instance Data.Aeson.ToJSON SessionId
instance Data.Aeson.FromJSON SessionId
instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary

sessionId :: String -> SessionId
sessionId = SessionId

newtype SequenceNum = SequenceNum Int deriving (Generic, Eq, Show, Read)
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

