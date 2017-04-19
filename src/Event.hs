------------------
-- |
-- Module: Event
--
-- Event description and functions.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Event
( Event(..)
, Hash
, Channel, channelOptions
, SourceId, sourceId, sourceIdOptions
, SessionId, sessionId
, sequenceNum
, nextSequenceNum
, sizeOf
, hash
, now
) where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (encode, decode)
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson.Types
    ( ToJSON, FromJSON, toJSON, parseJSON, Value(Object)
    , (.:), (.=), typeMismatch, object)
import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (pack, unpack)
import Data.Monoid ((<>))
import Data.Time
    ( UTCTime(UTCTime), getCurrentTime, fromGregorian, picosecondsToDiffTime
    , utctDay, utctDayTime, toGregorian, diffTimeToPicoseconds
    , picosecondsToDiffTime)
import GHC.Generics (Generic)
import qualified Options.Applicative as Opt
import System.Clock
    ( TimeSpec(TimeSpec), Clock(Boottime), getTime, toNanoSecs, fromNanoSecs
    , sec, nsec)
import Test.QuickCheck (Arbitrary, arbitrary, getPositive, choose)
import Text.Read (readMaybe)

-- local imports
import qualified Encodings as Enc

newtype Hash = Hash String deriving (Generic, Eq, Show, Read)

newtype Channel = Channel String deriving (Generic, Eq, Show, Read)
instance Bin.Binary Channel
instance ToJSON Channel
instance FromJSON Channel
instance Arbitrary Channel where
    arbitrary = Channel <$> arbitrary

channelOptions :: Opt.Parser Channel
channelOptions = Channel <$> Opt.strOption
    ( Opt.long "channel"
   <> Opt.metavar "CH"
   <> Opt.help "Channel identifier"
    )

newtype SourceId = SourceId String deriving (Generic, Eq, Show, Read)
instance Bin.Binary SourceId
instance ToJSON SourceId
instance FromJSON SourceId
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

newtype SessionId = SessionId String deriving (Generic, Eq, Show, Read)
instance Bin.Binary SessionId
instance ToJSON SessionId
instance FromJSON SessionId
instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary

sessionId :: String -> SessionId
sessionId = SessionId

instance Bin.Binary UTCTime where
    put t = do
        Bin.put $ toGregorian $ utctDay t
        Bin.put $ diffTimeToPicoseconds $ utctDayTime t

    get = do
        (a,b,c) <- Bin.get
        ps <- Bin.get
        return $ UTCTime (fromGregorian a b c) (picosecondsToDiffTime ps)

instance Bin.Binary TimeSpec where
    put t = do
        Bin.put $ sec t
        Bin.put $ nsec t

    get = TimeSpec <$> Bin.get <*> Bin.get

-- | Sequence number that wraps around.
newtype SequenceNum = SequenceNum Int deriving (Generic, Eq, Show, Read)
instance Bin.Binary SequenceNum
instance ToJSON SequenceNum
instance FromJSON SequenceNum
instance Arbitrary SequenceNum where
    arbitrary = sequenceNum <$> choose (a,b) where
        SequenceNum a = minBound
        SequenceNum b = maxBound

instance Bounded SequenceNum where
    minBound = SequenceNum 0
    maxBound = SequenceNum (0x10000-1)

sequenceNum :: (Integral i) => i -> SequenceNum
sequenceNum i
    | i < fromIntegral a = error "value too small"
    | i > fromIntegral b = error "value too large"
    | otherwise = SequenceNum $ fromIntegral i
  where
    SequenceNum a = minBound
    SequenceNum b = maxBound

-- | Increment sequence number, respect maxBound.
nextSequenceNum :: SequenceNum -> SequenceNum
nextSequenceNum (SequenceNum sn)
    | sn == b = minBound
    | otherwise = sequenceNum $ succ sn
  where
    SequenceNum b = maxBound

data Event = Event
    { eChannel  :: Channel
    , eSourceId :: SourceId     -- id of the recorder
    , eUtcTime  :: UTCTime      -- capture utc time
    , eMonoTime :: TimeSpec     -- capture monotonic (boot) time
    , eSessionId :: SessionId   -- monotonic time is valid only within
                                -- the same session ID
    , eSequence :: SequenceNum  -- incrementing sequence number
    , eValue    :: BS.ByteString   -- the event value
    } deriving (Generic, Eq, Show, Read)

instance Bin.Binary Event

instance Arbitrary Event where
    arbitrary = Event
        <$> arbitrary
        <*> arbitrary
        <*> (UTCTime <$> day <*> diffT)
        <*> (TimeSpec
            <$> fmap getPositive arbitrary
            <*> choose (0, 999999999))
        <*> arbitrary
        <*> arbitrary
        <*> (BS.pack <$> arbitrary)
      where
        day = fromGregorian
            <$> fmap getPositive arbitrary
            <*> choose (1,12)
            <*> choose (1,31)
        diffT = picosecondsToDiffTime <$> choose (0, (24*3600*(10^(12::Int))-1))

instance ToJSON Event where
    toJSON (Event ch src utcTime monoTime ses seqNum val) = object
        [ "channel"     .= ch
        , "recorder"    .= src
        , "utcTime"     .= utcTime
        , "monoTime"    .= toNanoSecs monoTime
        , "session"     .= ses
        , "sequence"    .= seqNum
        , "data"        .= Enc.hexlify val
        ]

instance FromJSON Event where
    parseJSON (Object v) = Event
        <$> v .: "channel"
        <*> v .: "recorder"
        <*> v .: "utcTime"
        <*> fmap fromNanoSecs (v .: "monoTime")
        <*> v .: "session"
        <*> v .: "sequence"
        <*> readStr (v .: "data")
      where
        readStr px = do
            s <- px
            maybe (fail "unable to parse") pure (Enc.unhexlify s)

    parseJSON invalid    = typeMismatch "Event" invalid

instance Enc.Encodable Event where

    -- | Encode single event.
    encode Enc.EncText evt = pack $ show evt
    encode (Enc.EncJSON Enc.JSONCompact) evt = BSL.toStrict $ encode evt
    encode (Enc.EncJSON (Enc.JSONPretty i)) e =
        BSL.toStrict $ AP.encodePretty'
        (AP.defConfig {AP.confCompare = compare, AP.confIndent = AP.Spaces i}) e
    encode Enc.EncBin evt = Enc.cobsEncode $ BSL.toStrict $ Bin.encode evt

    -- | Try to decode single event.
    decode Enc.EncText s = readMaybe $ unpack s
    decode (Enc.EncJSON _) s = decode $ BSL.fromStrict s
    decode Enc.EncBin s = do
        s' <- Enc.cobsDecode s
        let result = Bin.decodeOrFail $ BSL.fromStrict s'
        case result of
            Left _ -> Nothing
            Right (_,_,val) -> Just val

-- | Calculate size of event as size of it's eValue.
sizeOf :: Event -> Integer
sizeOf = fromIntegral . BS.length . eValue

-- hash event
hash :: Event -> Hash
hash (Event ch src utc mono ses seqNum val) = Hash $
    Enc.hexlify $ SHA256.finalize $ (flip SHA256.updates) parts $ SHA256.init
  where
    parts =
        [ pack . show $ ch
        , pack . show $ src
        , pack . show $ utc
        , pack . show $ mono
        , pack . show $ ses
        , pack . show $ seqNum
        , val
        ]

-- | Get current time (UTC,boot).
now :: IO (UTCTime, TimeSpec)
now = (,) <$> getCurrentTime <*> getTime Boottime

