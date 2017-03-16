{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Event where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (encode, decode)
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson.Types
    ( ToJSON, FromJSON, toJSON, parseJSON, Value(Object)
    , (.:), (.=), typeMismatch, object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (pack, unpack)
import Data.Time
    ( UTCTime(UTCTime), getCurrentTime, fromGregorian, secondsToDiffTime)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Clock
    ( TimeSpec(TimeSpec), Clock(Boottime), getTime, toNanoSecs, fromNanoSecs)
import Test.QuickCheck (Arbitrary, arbitrary, choose, oneof, getPositive)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Numeric (readHex)

-- | JSON format variants
data JSONFormat
    = JSONCompact       -- one line
    | JSONPretty Int    -- multiple lines with specified indent size
    deriving (Generic, Eq, Show)

instance Arbitrary JSONFormat where
    arbitrary = oneof
        [ pure JSONCompact
        , JSONPretty <$> choose (1,8)
        ]

-- | File encoding formats
data EncodeFormat
    = EncShow
    | EncJSON JSONFormat
    {-  TODO: support other formats
    | EncText Format
    | EncBSON
    | EncYAML Indent
    | EncMessagePack    -- Use COBS encoding algorithm for this binary format
    | EncXML
    -}
    deriving (Generic, Eq, Show)

instance Arbitrary EncodeFormat where
    arbitrary = oneof
        [ pure EncShow
        , EncJSON <$> arbitrary
        ]

newtype Hash = Hash String deriving (Generic, Eq, Show, Read)

newtype Chanel = Chanel String deriving (Generic, Eq, Show, Read)
instance ToJSON Chanel
instance FromJSON Chanel
instance Arbitrary Chanel where
    arbitrary = Chanel <$> arbitrary

newtype SourceId = SourceId String deriving (Generic, Eq, Show, Read)
instance ToJSON SourceId
instance FromJSON SourceId
instance Arbitrary SourceId where
    arbitrary = SourceId <$> arbitrary

newtype SessionId = SessionId String deriving (Generic, Eq, Show, Read)
instance ToJSON SessionId
instance FromJSON SessionId
instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary

data Event = Event
    { eChanel   :: Chanel
    , eSourceId :: SourceId     -- id of the recorder
    , eUtcTime  :: UTCTime      -- capture utc time
    , eBootTime :: TimeSpec     -- capture boot (monotonic) time
    , eSessionId :: SessionId   -- boot time is valid only within
                                -- the same session ID
    , eValue    :: BS.ByteString   -- the event value
    } deriving (Generic, Eq, Show, Read)

instance Arbitrary Event where
    arbitrary = Event
        <$> arbitrary
        <*> arbitrary
        <*> (UTCTime <$> day <*> diffT)
        <*> (TimeSpec
            <$> fmap getPositive arbitrary
            <*> choose (0, 999999999))
        <*> arbitrary
        <*> (BS.pack <$> arbitrary)
      where
        day = fromGregorian
            <$> fmap getPositive arbitrary
            <*> choose (1,12)
            <*> choose (1,31)
        diffT = secondsToDiffTime <$> choose (0, 86401)

instance ToJSON Event where
    toJSON (Event ch src utcTime bootTime ses val) = object
        [ "ch" .= ch
        , "src" .= src
        , "utc" .= utcTime
        , "boot" .= toNanoSecs bootTime
        , "session" .= ses
        , "value" .= hexlify val
        ]

instance FromJSON Event where
    parseJSON (Object v) = Event <$>
        v .: "ch" <*>
        v .: "src" <*>
        v .: "utc" <*>
        fmap fromNanoSecs (v .: "boot") <*>
        v .: "session" <*>
        readStr (v .: "value")
      where
        readStr px = do
            s <- px
            maybe (fail "unable to parse") pure (unhexlify s)

    parseJSON invalid    = typeMismatch "Event" invalid

-- | Convert bytestring to hex representation.
hexlify :: BS.ByteString -> String
hexlify = foldr (++) "" . map (printf "%02X") . BS.unpack

-- | Convert hex representation back to a bytestring.
unhexlify :: String -> Maybe BS.ByteString
unhexlify s = do
    nums <- getPairs [] s >>= sequence . map getNum
    return $ BS.pack $ reverse nums
  where
    getPairs acc [] = Just acc
    getPairs _ (_:[]) = Nothing
    getPairs acc (a:b:xs) = getPairs ([a,b]:acc) xs

    getNum x = case readHex x of
        [(a,"")] -> Just a
        _ -> Nothing

-- | hexlify and unhexlify shall be inverse operations
propHexlify :: [Word8] -> Bool
propHexlify lst = unhexlify (hexlify bs) == Just bs where
    bs = BS.pack lst

-- | Calculate size of event as size of it's eValue.
sizeOf :: Event -> Integer
sizeOf = fromIntegral . BS.length . eValue

-- hash event
hash :: Event -> Hash
hash e = Hash $
    hexlify $ SHA256.finalize $ (flip SHA256.updates) parts $ SHA256.init
  where
    parts =
        [ pack . show $ eChanel e
        , pack . show $ eSourceId e
        , pack . show $ eUtcTime e
        , pack . show $ eBootTime e
        , pack . show $ eSessionId e
        , eValue e
        ]

-- | Get current time (UTC,boot).
now :: IO (UTCTime, TimeSpec)
now = (,) <$> getCurrentTime <*> getTime Boottime

-- | Event delimiter for different encoding formats.
delimit :: EncodeFormat -> String
delimit EncShow = "\n"
delimit (EncJSON _) = "\n"

-- | Encode single event.
encodeEvent :: EncodeFormat -> Event -> BS.ByteString
encodeEvent EncShow evt = pack $ show evt
encodeEvent (EncJSON JSONCompact) evt = BSL.toStrict $ encode evt
encodeEvent (EncJSON (JSONPretty i)) evt = BSL.toStrict $ AP.encodePretty'
    (AP.defConfig {AP.confCompare = compare, AP.confIndent = AP.Spaces i}) evt

-- | Encode multiple events.
encodeEvents :: EncodeFormat -> [Event] -> BS.ByteString
encodeEvents fmt lst = foldr mappend BS.empty
    [encodeEvent fmt e `mappend` pack (delimit fmt) | e <- lst]

-- | Try to decode single event.
decodeEvent :: EncodeFormat -> BS.ByteString -> Maybe Event
decodeEvent EncShow = readMaybe . unpack
decodeEvent (EncJSON _) = decode . BSL.fromStrict

-- | Try to decode multiple events.
decodeEvents :: EncodeFormat -> BS.ByteString -> Maybe [Event]
decodeEvents fmt s
    | BS.null s = Just []
    | otherwise = do
        (e,s') <- tryDecode s
        rest <- decodeEvents fmt s'
        return $ e:rest
  where
    delim = pack $ delimit fmt

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

-- | encodeEvent and decodeEvent shall be inverse operations.
propEncodeDecode :: EncodeFormat -> Event -> Bool
propEncodeDecode fmt e = decodeEvent fmt (encodeEvent fmt e) == Just e

-- | encodeEvents and decodeEvents shall be inverse operations.
propEncodeDecodeMulti :: EncodeFormat -> [Event]-> Bool
propEncodeDecodeMulti fmt lst =
    decodeEvents fmt (encodeEvents fmt lst) == Just lst

