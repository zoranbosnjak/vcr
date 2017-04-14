{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Event where

import qualified Crypto.Hash.SHA256 as SHA256
import Control.Monad (guard)
import Data.Aeson (encode, decode)
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson.Types
    ( ToJSON, FromJSON, toJSON, parseJSON, Value(Object)
    , (.:), (.=), typeMismatch, object)
import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (pack, unpack)
import Data.Time
    ( UTCTime(UTCTime), getCurrentTime, fromGregorian, picosecondsToDiffTime
    , utctDay, utctDayTime, toGregorian, diffTimeToPicoseconds
    , picosecondsToDiffTime)
import GHC.Generics (Generic)
import Numeric (readHex)
import System.Clock
    ( TimeSpec(TimeSpec), Clock(Boottime), getTime, toNanoSecs, fromNanoSecs
    , sec, nsec)
import Test.QuickCheck (Arbitrary, arbitrary, getPositive, choose, oneof)
import Text.Printf (printf)
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

newtype SourceId = SourceId String deriving (Generic, Eq, Show, Read)
instance Bin.Binary SourceId
instance ToJSON SourceId
instance FromJSON SourceId
instance Arbitrary SourceId where
    arbitrary = SourceId <$> arbitrary

newtype SessionId = SessionId String deriving (Generic, Eq, Show, Read)
instance Bin.Binary SessionId
instance ToJSON SessionId
instance FromJSON SessionId
instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary

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

-- | Encode single event.
encodeEvent :: Enc.EncodeFormat -> Event -> BS.ByteString
encodeEvent Enc.EncShow evt = pack $ show evt
encodeEvent (Enc.EncJSON Enc.JSONCompact) evt = BSL.toStrict $ encode evt
encodeEvent (Enc.EncJSON (Enc.JSONPretty i)) evt =
    BSL.toStrict $ AP.encodePretty'
    (AP.defConfig {AP.confCompare = compare, AP.confIndent = AP.Spaces i}) evt
encodeEvent Enc.EncBin evt = Enc.cobsEncode $ BSL.toStrict $ Bin.encode evt

-- | Encode multiple events.
encodeEvents :: Enc.EncodeFormat -> [Event] -> BS.ByteString
encodeEvents fmt lst = foldr mappend BS.empty
    [encodeEvent fmt e `mappend` pack (Enc.delimit fmt) | e <- lst]

-- | Try to decode single event.
decodeEvent :: Enc.EncodeFormat -> BS.ByteString -> Maybe Event
decodeEvent Enc.EncShow s = readMaybe $ unpack s
decodeEvent (Enc.EncJSON _) s = decode $ BSL.fromStrict s
decodeEvent Enc.EncBin s = do
    s' <- Enc.cobsDecode s
    let result = Bin.decodeOrFail $ BSL.fromStrict s'
    case result of
        Left _ -> Nothing
        Right (_,_,val) -> Just val

-- | Try to decode multiple events.
decodeEvents :: Enc.EncodeFormat -> BS.ByteString -> Maybe [Event]
decodeEvents fmt s
    | BS.null s = Just []
    | otherwise = do
        (e,s') <- tryDecode s
        rest <- decodeEvents fmt s'
        return $ e:rest
  where
    delim = pack $ Enc.delimit fmt

    getProbes probe x = (probe,x) : case BS.null x of
        True -> []
        False -> getProbes
            (BS.concat [probe,a]) (BS.drop (BS.length delim) b)
      where
        (a,b) = BS.breakSubstring delim x

    tryDecode x = do
        let probes = getProbes BS.empty x
            firstOf [] = Nothing
            firstOf ((Nothing,_):rest) = firstOf rest
            firstOf ((Just a,b):_) = Just (a,b)
        firstOf [(decodeEvent fmt a,b) | (a,b) <- probes]

