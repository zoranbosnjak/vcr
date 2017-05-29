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
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson.Types (typeMismatch, object, (.=), (.:))
import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Monoid ((<>))
import qualified Data.Time
import GHC.Generics (Generic)
import qualified Options.Applicative as Opt
import qualified System.Clock
import Test.QuickCheck (Arbitrary, arbitrary, getPositive, choose)
import Text.Read (readMaybe)

-- local imports
import qualified Encodings as Enc

newtype Hash = Hash String deriving (Generic, Eq, Show, Read)

newtype Channel = Channel String deriving (Generic, Eq, Show, Read)
instance Bin.Binary Channel
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
instance Bin.Binary SourceId
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

newtype SessionId = SessionId String deriving (Generic, Eq, Show, Read)
instance Bin.Binary SessionId
instance Data.Aeson.ToJSON SessionId
instance Data.Aeson.FromJSON SessionId
instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary

sessionId :: String -> SessionId
sessionId = SessionId

-- Make UTC time an (orphan) instance of Binary.
instance Bin.Binary Data.Time.UTCTime where
    put t = do
        Bin.put $ Data.Time.toGregorian $ Data.Time.utctDay t
        Bin.put $ Data.Time.diffTimeToPicoseconds $ Data.Time.utctDayTime t

    get = do
        (a,b,c) <- Bin.get
        ps <- Bin.get
        return $ Data.Time.UTCTime
            (Data.Time.fromGregorian a b c) (Data.Time.picosecondsToDiffTime ps)

-- Make TimeSpec an (orphan) instance of Binary.
instance Bin.Binary System.Clock.TimeSpec where
    put t = do
        Bin.put $ System.Clock.sec t
        Bin.put $ System.Clock.nsec t

    get = System.Clock.TimeSpec <$> Bin.get <*> Bin.get

-- | Sequence number that wraps around.
newtype SequenceNum = SequenceNum Int deriving (Generic, Eq, Show, Read)
instance Bin.Binary SequenceNum
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

data Event = Event
    { eChannel  :: Channel              -- name of the channel
    , eSourceId :: SourceId             -- id of the recorder
    , eUtcTime  :: Data.Time.UTCTime    -- capture utc time
    , eMonoTime :: System.Clock.TimeSpec -- capture monotonic (boot) time
    , eSessionId :: SessionId           -- monotonic time is valid only within
                                        -- the same session ID
    , eSequence :: SequenceNum          -- incrementing sequence number
    , eValue    :: BS.ByteString        -- the event value
    } deriving (Generic, Eq, Show, Read)

instance Bin.Binary Event

instance Arbitrary Event where
    arbitrary = Event
        <$> arbitrary
        <*> arbitrary
        <*> (Data.Time.UTCTime <$> day <*> diffT)
        <*> (System.Clock.TimeSpec
            <$> fmap getPositive arbitrary
            <*> choose (0, 999999999))
        <*> arbitrary
        <*> arbitrary
        <*> (BS.pack <$> arbitrary)
      where
        day = Data.Time.fromGregorian
            <$> fmap getPositive arbitrary
            <*> choose (1,12)
            <*> choose (1,31)
        diffT = Data.Time.picosecondsToDiffTime
            <$> choose (0, (24*3600*(10^(12::Int))-1))

instance Data.Aeson.ToJSON Event where
    toJSON (Event ch src utcTime monoTime ses seqNum val) = object
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
        <*> v .: "utcTime"
        <*> fmap System.Clock.fromNanoSecs (v .: "monoTime")
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
    encode Enc.EncText evt = BSL8.pack $ show evt
    encode (Enc.EncJSON Enc.JSONCompact) evt = Data.Aeson.encode evt
    encode (Enc.EncJSON (Enc.JSONPretty i)) e = AP.encodePretty'
        (AP.defConfig {AP.confCompare = compare, AP.confIndent = AP.Spaces i}) e
    encode Enc.EncBin evt = Enc.cobsEncode $ Bin.encode evt

    -- | Try to decode single event.
    decode Enc.EncText s = readMaybe $ BSL8.unpack s
    decode (Enc.EncJSON _) s = Data.Aeson.decode s
    decode Enc.EncBin s = do
        s' <- Enc.cobsDecode s
        let result = Bin.decodeOrFail s'
        case result of
            Left _ -> Nothing
            Right (_,_,val) -> Just val

-- | Calculate size of event as a size of it's eValue.
sizeOf :: Event -> Integer
sizeOf = fromIntegral . BS.length . eValue

-- hash event
hash :: Event -> Hash
hash (Event ch src utc mono ses seqNum val) = Hash $
    Enc.hexlify $ SHA256.finalize $ (flip SHA256.updates) parts $ SHA256.init
  where
    parts =
        [ BS8.pack . show $ ch
        , BS8.pack . show $ src
        , BS8.pack . show $ utc
        , BS8.pack . show $ mono
        , BS8.pack . show $ ses
        , BS8.pack . show $ seqNum
        , val
        ]

-- | Get current time (UTC,boot).
now :: IO (Data.Time.UTCTime, System.Clock.TimeSpec)
now = (,)
    <$> Data.Time.getCurrentTime
    <*> System.Clock.getTime System.Clock.Boottime

