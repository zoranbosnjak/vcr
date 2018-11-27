------------------
-- |
-- Module: Event
--
-- Event description and functions.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Event where

import           Data.String
import qualified Data.Aeson
import           Data.Aeson.Types (typeMismatch, object, (.=), (.:))
import qualified Data.Serialize as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as B64
import           Data.Monoid ((<>))
import           Control.DeepSeq

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.ToRow as PGTR
import qualified Database.PostgreSQL.Simple.FromRow as PGFR
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.FromField as PGFF

import qualified Database.SQLite.Simple as SL
import qualified Database.SQLite.Simple.ToRow as SLTR
import qualified Database.SQLite.Simple.FromRow as SLFR
import qualified Database.SQLite.Simple.ToField as SLTF
import qualified Database.SQLite.Simple.FromField as SLFF

import           Text.Read
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
    , eValue    :: Payload              -- the event value
    } deriving (Generic, Eq, Show, Read)

instance NFData Event

instance PGTR.ToRow Event where
    toRow evt =
        [ PGTF.toField $ eChannel evt
        , PGTF.toField $ eSourceId evt
        , PGTF.toField $ eUtcTime evt
        , PGTF.toField (getUtcPicos $ eUtcTime evt)
        , PGTF.toField $ (System.Clock.sec $ getMonoTime $ eMonoTime evt)
        , PGTF.toField $ (System.Clock.nsec $ getMonoTime $ eMonoTime evt)
        , PGTF.toField $ eSessionId evt
        , PGTF.toField $ eTrackId  evt
        , PGTF.toField $ eSequence evt
        , PGTF.toField $ PG.Binary $ getPayload $ eValue evt
        ]

instance SLTR.ToRow Event where
    toRow evt =
        [ SLTF.toField $ eChannel evt
        , SLTF.toField $ eSourceId evt
        , SLTF.toField $ eUtcTime evt
        , SLTF.toField (getUtcPicos $ eUtcTime evt)
        , SLTF.toField $ (System.Clock.sec $ getMonoTime $ eMonoTime evt)
        , SLTF.toField $ (System.Clock.nsec $ getMonoTime $ eMonoTime evt)
        , SLTF.toField $ eSessionId evt
        , SLTF.toField $ eTrackId  evt
        , SLTF.toField $ eSequence evt
        , SLTF.toField $ eValue evt
        ]

instance PGFR.FromRow Event where
    fromRow = Event
        <$> PGFR.field
        <*> PGFR.field
        <*> do
            a <- PGFR.field
            b <- PGFR.field
            return $ replaceUtcPicos b a
        <*> do
            a <- PGFR.field
            b <- PGFR.field
            return $ MonoTime $ System.Clock.TimeSpec a b
        <*> PGFR.field
        <*> PGFR.field
        <*> PGFR.field
        <*> PGFR.field

instance SLFR.FromRow Event where
    fromRow = Event
        <$> SLFR.field
        <*> SLFR.field
        <*> do
            a <- SLFR.field
            b <- SLFR.field
            return $ replaceUtcPicos b a
        <*> do
            a <- SLFR.field
            b <- SLFR.field
            return $ MonoTime $ System.Clock.TimeSpec a b
        <*> SLFR.field
        <*> SLFR.field
        <*> SLFR.field
        <*> SLFR.field

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
        <*> arbitrary

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
        , "data"        .= Enc.hexlify (getPayload val)
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
        <*> fmap Payload (readStr (v .: "data"))
      where
        readStr px = do
            s <- px
            maybe (fail "unable to parse") pure (Enc.unhexlify s)

    parseJSON invalid    = typeMismatch "Event" invalid

newtype Channel = Channel { getChannel :: Text }
    deriving (Generic, Eq, Ord)
instance Show Channel where
    show (Channel val) = show val
instance Read Channel where
    readPrec = Channel <$> readPrec
instance NFData Channel
instance Bin.Serialize Channel where
    put (Channel c) = Bin.put $ TE.encodeUtf8 c
    get = Channel . TE.decodeUtf8 <$> Bin.get
instance Data.Aeson.ToJSON Channel where
    toJSON (Channel val) = Data.Aeson.toJSON val
instance Data.Aeson.FromJSON Channel where
    parseJSON val = Channel <$> Data.Aeson.parseJSON val
instance Data.Aeson.ToJSONKey Channel
instance Data.Aeson.FromJSONKey Channel
instance Arbitrary Channel where
    arbitrary = Channel . T.pack <$> arbitrary
instance IsString Channel where
    fromString s = Channel $ fromString s
instance PGTF.ToField Channel where
    toField (Channel val) = PGTF.toField val
instance PGFF.FromField Channel where
    fromField f mdata = Channel <$> PGFF.fromField f mdata
instance SLTF.ToField Channel where
    toField (Channel val) = SLTF.toField val
instance SLFF.FromField Channel where
    fromField f = Channel <$> SLFF.fromField f

channelOptions :: Opt.Parser Channel
channelOptions = Channel <$> Opt.strOption
    ( Opt.long "channel"
   <> Opt.metavar "CH"
   <> Opt.help "Channel identifier"
    )

newtype SourceId = SourceId { getSourceId :: Text }
    deriving (Generic, Eq)
instance Show SourceId where
    show (SourceId val) = show val
instance Read SourceId where
    readPrec = SourceId <$> readPrec
instance NFData SourceId
instance Bin.Serialize SourceId where
    put (SourceId c) = Bin.put $ TE.encodeUtf8 c
    get = SourceId . TE.decodeUtf8 <$> Bin.get
instance Data.Aeson.ToJSON SourceId where
    toJSON (SourceId val) = Data.Aeson.toJSON val
instance Data.Aeson.FromJSON SourceId where
    parseJSON val = SourceId <$> Data.Aeson.parseJSON val
instance Arbitrary SourceId where
    arbitrary = SourceId . T.pack <$> arbitrary
instance IsString SourceId where
    fromString s = SourceId $ fromString s
instance PGTF.ToField SourceId where
    toField (SourceId val) = PGTF.toField val
instance PGFF.FromField SourceId where
    fromField f mdata = SourceId <$> PGFF.fromField f mdata
instance SLTF.ToField SourceId where
    toField (SourceId val) = SLTF.toField val
instance SLFF.FromField SourceId where
    fromField f = SourceId <$> SLFF.fromField f

sourceId :: Text -> SourceId
sourceId = SourceId

sourceIdOptions :: Opt.Parser SourceId
sourceIdOptions = SourceId <$> Opt.strOption
    ( Opt.long "ident"
   <> Opt.metavar "IDENT"
   <> Opt.help "Recorder identifier"
    )

newtype UtcTime = UtcTime { getUtcTime :: Data.Time.UTCTime }
    deriving (Generic, Eq, Ord)
instance Show UtcTime where
    show (UtcTime t) = show t
instance Read UtcTime where
    readPrec = UtcTime <$> readPrec
instance Data.Aeson.ToJSON UtcTime
instance Data.Aeson.FromJSON UtcTime
instance PGTF.ToField UtcTime where
    toField (UtcTime val) = PGTF.toField val
instance PGFF.FromField UtcTime where
    fromField f mdata = UtcTime <$> PGFF.fromField f mdata
instance SLTF.ToField UtcTime where
    toField (UtcTime val) = SLTF.toField val
instance SLFF.FromField UtcTime where
    fromField f = UtcTime <$> SLFF.fromField f
instance NFData UtcTime

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
    deriving (Generic, Eq, Ord)
instance Show MonoTime where
    show (MonoTime val) = show val
instance Read MonoTime where
    readPrec = MonoTime <$> readPrec
instance NFData MonoTime where
    rnf (MonoTime t) =
        System.Clock.sec t `deepseq` System.Clock.nsec t `deepseq` ()

instance Arbitrary MonoTime where
    arbitrary = do
        a <- fmap getPositive arbitrary
        b <- choose (0, 999999999)
        return $ MonoTime $ System.Clock.TimeSpec a b

instance Bin.Serialize MonoTime where
    put (MonoTime t) = do
        Bin.put $ System.Clock.sec t
        Bin.put $ System.Clock.nsec t

    get = do
        a <- Bin.get
        b <- Bin.get
        return $ MonoTime $ System.Clock.TimeSpec a b

newtype SessionId = SessionId { getSessionId :: String }
    deriving (Generic, Eq, Ord)
instance Show SessionId where
    show (SessionId val) = show val
instance Read SessionId where
    readPrec = SessionId <$> readPrec
instance NFData SessionId
instance Bin.Serialize SessionId
instance Data.Aeson.ToJSON SessionId where
    toJSON (SessionId val) = Data.Aeson.toJSON val
instance Data.Aeson.FromJSON SessionId where
    parseJSON val = SessionId <$> Data.Aeson.parseJSON val
instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary
instance IsString SessionId where
    fromString = SessionId
instance PGTF.ToField SessionId where
    toField (SessionId val) = PGTF.toField val
instance PGFF.FromField SessionId where
    fromField f mdata = SessionId <$> PGFF.fromField f mdata
instance SLTF.ToField SessionId where
    toField (SessionId val) = SLTF.toField val
instance SLFF.FromField SessionId where
    fromField f = SessionId <$> SLFF.fromField f

sessionId :: String -> SessionId
sessionId = SessionId

newtype TrackId = TrackId { getTrackId :: String }
    deriving (Generic, Eq, Ord)
instance Show TrackId where
    show (TrackId val) = show val
instance Read TrackId where
    readPrec = TrackId <$> readPrec
instance NFData TrackId
instance Bin.Serialize TrackId
instance Data.Aeson.ToJSON TrackId where
    toJSON (TrackId val) = Data.Aeson.toJSON val
instance Data.Aeson.FromJSON TrackId where
    parseJSON val = TrackId <$> Data.Aeson.parseJSON val
instance Arbitrary TrackId where
    arbitrary = TrackId <$> arbitrary
instance IsString TrackId where
    fromString = TrackId
instance PGTF.ToField TrackId where
    toField (TrackId val) = PGTF.toField val
instance PGFF.FromField TrackId where
    fromField f mdata = TrackId <$> PGFF.fromField f mdata
instance SLTF.ToField TrackId where
    toField (TrackId val) = SLTF.toField val
instance SLFF.FromField TrackId where
    fromField f = TrackId <$> SLFF.fromField f

trackId :: String -> TrackId
trackId = TrackId

-- | Sequence number that wraps around.
data SequenceNum = SequenceNum { getSequenceNum :: !Integer }
    deriving (Generic, Eq)
instance Show SequenceNum where
    show (SequenceNum val) = show val
instance Read SequenceNum where
    readPrec = SequenceNum <$> readPrec
instance NFData SequenceNum
instance Bin.Serialize SequenceNum
instance Data.Aeson.ToJSON SequenceNum where
    toJSON (SequenceNum val) = Data.Aeson.toJSON val
instance Data.Aeson.FromJSON SequenceNum where
    parseJSON val = SequenceNum <$> Data.Aeson.parseJSON val
instance PGTF.ToField SequenceNum where
    toField (SequenceNum val) = PGTF.toField val
instance PGFF.FromField SequenceNum where
    fromField f mdata = SequenceNum <$> PGFF.fromField f mdata
instance SLTF.ToField SequenceNum where
    toField (SequenceNum val) = SLTF.toField val
instance SLFF.FromField SequenceNum where
    fromField f = SequenceNum <$> SLFF.fromField f

instance Bounded SequenceNum where
    minBound = SequenceNum 0
    maxBound = SequenceNum (0x10000-1)

-- | Smart constructor for SequenceNum
sequenceNum :: (Integral i) => i -> SequenceNum
sequenceNum !i
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

newtype Payload = Payload { getPayload :: BS.ByteString }
    deriving (Generic, Eq)
instance NFData Payload
instance Bin.Serialize Payload
instance PGTF.ToField Payload where
    toField (Payload val) = PGTF.toField val
instance PGFF.FromField Payload where
    fromField f mdata = Payload <$> PGFF.fromField f mdata
instance SLTF.ToField Payload where
    toField (Payload val) = SLTF.toField val
instance SLFF.FromField Payload where
    fromField f = Payload <$> SLFF.fromField f
instance Show Payload where
    show (Payload s) = show (Enc.hexlify s)
instance Read Payload where
    readPrec = do
        s <- readPrec
        case Enc.unhexlify s of
            Nothing -> error "can not parse payload"
            Just x -> return $ Payload x
instance Arbitrary Payload where
    arbitrary = Payload . BS.pack <$> resize 1000 arbitrary

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
            , eValue =
                Payload $ BS.take lnLimit $ B64.encode $ BS8.pack $ show e
            }
    e0 = Event
        ch
        rec
        (UtcTime startTime)
        (MonoTime $ System.Clock.TimeSpec 0 0)
        sid
        tid
        minBound
        (Payload $ B64.encode "testData")

