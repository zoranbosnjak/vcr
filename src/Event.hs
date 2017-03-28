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

-- | File encoding formats (use COBS encoding for binary formats)
data EncodeFormat
    = EncShow
    | EncJSON JSONFormat
    | EncBin
    deriving (Generic, Eq, Show)

instance Arbitrary EncodeFormat where
    arbitrary = oneof
        [ pure EncShow
        , EncJSON <$> arbitrary
        , pure EncBin
        ]

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

data Event = Event
    { eChannel  :: Channel
    , eSourceId :: SourceId     -- id of the recorder
    , eUtcTime  :: UTCTime      -- capture utc time
    , eBootTime :: TimeSpec     -- capture boot (monotonic) time
    , eSessionId :: SessionId   -- boot time is valid only within
                                -- the same session ID
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
        <*> (BS.pack <$> arbitrary)
      where
        day = fromGregorian
            <$> fmap getPositive arbitrary
            <*> choose (1,12)
            <*> choose (1,31)
        diffT = picosecondsToDiffTime <$> choose (0, (24*3600*(10^(12::Int))-1))

instance ToJSON Event where
    toJSON (Event ch src utcTime bootTime ses val) = object
        [ "channel"     .= ch
        , "recorder"    .= src
        , "utcTime"     .= utcTime
        , "monoTime"    .= toNanoSecs bootTime
        , "session"     .= ses
        , "data"        .= hexlify val
        ]

instance FromJSON Event where
    parseJSON (Object v) = Event <$>
        v .: "channel" <*>
        v .: "recorder" <*>
        v .: "utcTime" <*>
        fmap fromNanoSecs (v .: "monoTime") <*>
        v .: "session" <*>
        readStr (v .: "data")
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

-- | COBS encode single binary string
cobsEncode :: BS.ByteString -> BS.ByteString
cobsEncode s = BS.concat $ encodeSegments s where
    encodeSegments x = BS.singleton prefix : chunk : rest where
        (a,b) = BS.span (/= 0) x
        chunk = BS.take 254 a
        n = BS.length chunk
        prefix = fromIntegral $ succ n
        c = BS.drop n a `BS.append` b
        rest = case BS.null c of
            True -> []
            False -> case prefix of
                255 -> encodeSegments c
                _ -> encodeSegments $ BS.drop 1 c

-- | COBS decode single binary string.
cobsDecode :: BS.ByteString -> Maybe BS.ByteString
cobsDecode s = do
    (a,b) <- decodeSegment s
    case BS.null b of
        True -> return a
        False -> do
            rest <- cobsDecode b
            return $ a `BS.append` rest
  where
    decodeSegment :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
    decodeSegment x = do
        (a,b) <- BS.uncons x
        guard $ a > 0
        let n = pred $ fromIntegral a
            (c,d) = BS.splitAt n b
        guard $ BS.length c == n
        guard $ BS.findIndex (== 0) c == Nothing
        case BS.null d of
            True -> return (c, d)
            False -> case a of
                255 -> do
                    (e,f) <- decodeSegment d
                    return (c `BS.append` e, f)
                _ -> return (c `BS.append` BS.singleton 0, d)

-- | Calculate size of event as size of it's eValue.
sizeOf :: Event -> Integer
sizeOf = fromIntegral . BS.length . eValue

-- hash event
hash :: Event -> Hash
hash e = Hash $
    hexlify $ SHA256.finalize $ (flip SHA256.updates) parts $ SHA256.init
  where
    parts =
        [ pack . show $ eChannel e
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
delimit EncBin = "\0"

-- | Encode single event.
encodeEvent :: EncodeFormat -> Event -> BS.ByteString
encodeEvent EncShow evt = pack $ show evt
encodeEvent (EncJSON JSONCompact) evt = BSL.toStrict $ encode evt
encodeEvent (EncJSON (JSONPretty i)) evt = BSL.toStrict $ AP.encodePretty'
    (AP.defConfig {AP.confCompare = compare, AP.confIndent = AP.Spaces i}) evt
encodeEvent EncBin evt = cobsEncode $ BSL.toStrict $ Bin.encode evt

-- | Encode multiple events.
encodeEvents :: EncodeFormat -> [Event] -> BS.ByteString
encodeEvents fmt lst = foldr mappend BS.empty
    [encodeEvent fmt e `mappend` pack (delimit fmt) | e <- lst]

-- | Try to decode single event.
decodeEvent :: EncodeFormat -> BS.ByteString -> Maybe Event
decodeEvent EncShow s = readMaybe $ unpack s
decodeEvent (EncJSON _) s = decode $ BSL.fromStrict s
decodeEvent EncBin s = do
    s' <- cobsDecode s
    let result = Bin.decodeOrFail $ BSL.fromStrict s'
    case result of
        Left _ -> Nothing
        Right (_,_,val) -> Just val

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

