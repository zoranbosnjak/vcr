------------------
-- |
-- Module: Event
--
-- Event description and functions.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Event where

import           Data.String
import qualified Data.Aeson
import           Data.Aeson.Types (typeMismatch, object, (.=), (.:))
import qualified Data.Serialize as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as B64
import           Data.Monoid ((<>))
import           Database.HDBC
import Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.ToRow as PGSTR
import Database.PostgreSQL.Simple.FromRow as PGSFR
import Database.PostgreSQL.Simple.ToField as PGSTF
import Database.PostgreSQL.Simple.FromField as PGSFF
import           Data.Convertible
import qualified Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Options.Applicative as Opt
import qualified System.Clock
import           Test.QuickCheck
                    (Arbitrary, arbitrary, getPositive, choose, resize)

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

instance PGSTR.ToRow Event where
    toRow evt =
        [ toField $ eChannel evt
        , toField $ eSourceId evt
        , toField $ eUtcTime evt
        , toField (getUtcPicos $ eUtcTime evt)
        , toField $ (System.Clock.sec $ getMonoTime $ eMonoTime evt)
        , toField $ (System.Clock.nsec $ getMonoTime $ eMonoTime evt)
        , toField $ eSessionId evt
        , toField $ eTrackId  evt
        , toField $ eSequence evt
        , toField $ PGSimple.Binary $ eValue evt
        ]

instance PGSFR.FromRow Event where
    fromRow = Event
        <$> field
        <*> field
        <*> do
            a <- field
            b <- field
            return $ replaceUtcPicos b a
        <*> do
            a <- field
            b <- field
            return $ MonoTime $ System.Clock.TimeSpec a b
        <*> field
        <*> field
        <*> field
        <*> field

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
        <*> (BS.pack <$> resize 1000 arbitrary)

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
instance IsString Channel where
    fromString s = Channel $ fromString s
instance PGSTF.ToField Channel where
    toField (Channel val) = toField val
instance PGSFF.FromField Channel where
    fromField f mdata = Channel <$> fromField f mdata

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
instance IsString SourceId where
    fromString s = SourceId $ fromString s
instance PGSTF.ToField SourceId where
    toField (SourceId val) = toField val
instance PGSFF.FromField SourceId where
    fromField f mdata = SourceId <$> fromField f mdata

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
instance Data.Aeson.ToJSON UtcTime
instance Data.Aeson.FromJSON UtcTime

instance PGSTF.ToField UtcTime where
    toField (UtcTime val) = toField val

instance PGSFF.FromField UtcTime where
    fromField f mdata = UtcTime <$> fromField f mdata

instance Arbitrary UtcTime where
    arbitrary = do
        a <- day
        b <- diffT
        return $ UtcTime $ Data.Time.UTCTime a b
      where
        day = Data.Time.fromGregorian
            <$> choose (1000, 9999)
            <*> choose (1,12)
            <*> choose (1,31)
        diffT = Data.Time.picosecondsToDiffTime
            <$> choose (0, (24*3600*(10^(12::Int))-1))

instance Convertible UtcTime SqlValue where
    safeConvert (UtcTime val) = Right $ SqlUTCTime val

instance Convertible SqlValue UtcTime where
    safeConvert val = UtcTime <$> safeConvert val

instance Bin.Serialize UtcTime where
    put (UtcTime t) = do
        Bin.put $ Data.Time.toGregorian $ Data.Time.utctDay t
        Bin.put $ Data.Time.diffTimeToPicoseconds $ Data.Time.utctDayTime t

    get = do
        (a,b,c) <- Bin.get
        ps <- Bin.get
        return $ UtcTime $ Data.Time.UTCTime
            (Data.Time.fromGregorian a b c) (Data.Time.picosecondsToDiffTime ps)

getUtcPicos :: UtcTime -> Integer
getUtcPicos (UtcTime t) =
    Data.Time.diffTimeToPicoseconds $ Data.Time.utctDayTime t

replaceUtcPicos :: Integer -> UtcTime -> UtcTime
replaceUtcPicos p (UtcTime t) = UtcTime $ t { Data.Time.utctDayTime = dt } where
    dt = Data.Time.picosecondsToDiffTime p

newtype MonoTime = MonoTime { getMonoTime :: System.Clock.TimeSpec }
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

{-
instance PGSTF.ToField MonoTime where
    toField (MonoTime val) = toField $ System.Clock.toNanoSecs val

instance PGSFF.FromField MonoTime where
    fromField f mdata =
        MonoTime . System.Clock.fromNanoSecs <$> fromField f mdata
-}

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
instance IsString SessionId where
    fromString = SessionId

instance Convertible SessionId SqlValue where
    safeConvert (SessionId val) = Right $ SqlString val
instance Convertible SqlValue SessionId where
    safeConvert val = SessionId <$> safeConvert val

instance PGSTF.ToField SessionId where
    toField (SessionId val) = toField val

instance PGSFF.FromField SessionId where
    fromField f mdata = SessionId <$> fromField f mdata

sessionId :: String -> SessionId
sessionId = SessionId

newtype TrackId = TrackId String deriving (Generic, Eq, Ord, Show, Read)
instance Bin.Serialize TrackId
instance Data.Aeson.ToJSON TrackId
instance Data.Aeson.FromJSON TrackId
instance Arbitrary TrackId where
    arbitrary = TrackId <$> arbitrary
instance IsString TrackId where
    fromString = TrackId

instance Convertible TrackId SqlValue where
    safeConvert (TrackId val) = Right $ SqlString val
instance Convertible SqlValue TrackId where
    safeConvert val = TrackId <$> safeConvert val

instance PGSTF.ToField TrackId where
    toField (TrackId val) = toField val

instance PGSFF.FromField TrackId where
    fromField f mdata = TrackId <$> fromField f mdata

trackId :: String -> TrackId
trackId = TrackId

-- | Sequence number that wraps around.
newtype SequenceNum = SequenceNum Integer
    deriving (Generic, Eq, Show, Read)
instance Bin.Serialize SequenceNum
instance Data.Aeson.ToJSON SequenceNum
instance Data.Aeson.FromJSON SequenceNum

instance PGSTF.ToField SequenceNum where
    toField (SequenceNum val) = toField val

instance PGSFF.FromField SequenceNum where
    fromField f mdata = SequenceNum <$> fromField f mdata

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

-- | Infinite list of pseudo random events for test purposes.
randomEvents :: Rational -> Channel -> SourceId -> Data.Time.UTCTime
    -> SessionId -> TrackId -> Int -> [Event]
randomEvents dt ch rec startTime sid tid lnLimit = loop e0 where
    loop e = e:loop next where
        UtcTime utc = eUtcTime e
        MonoTime mono = eMonoTime e
        nextMono =
            let dtNano = round $ ((fromRational $ dt * 1000000000)::Double)
            in System.Clock.fromNanoSecs $ (+ dtNano) $
                System.Clock.toNanoSecs mono
        next = e
            { eUtcTime = UtcTime $ Data.Time.addUTCTime (fromRational dt) utc
            , eMonoTime = MonoTime nextMono
            , eSequence = nextSequenceNum $ eSequence e
            , eValue = BS.take lnLimit $ B64.encode $ BS8.pack $ show e
            }
    e0 = Event
        ch
        rec
        (UtcTime startTime)
        (MonoTime $ System.Clock.TimeSpec 0 0)
        sid
        tid
        minBound
        (B64.encode "testData")

