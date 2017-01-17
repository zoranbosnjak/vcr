{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Event where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Time
import GHC.Generics
import System.Clock
import Test.QuickCheck
import Text.Printf
import Numeric (readHex)

newtype Delimiter = Delimiter String deriving (Generic, Eq, Show)

data Format
    = FormatJSON
    | FormatJSONPretty
    | FormatBSON
    | FormatTXT
    deriving (Generic, Eq, Show)

{- TODO:
manual show (or other) instance, to be able to see
all choices in option parse
FormatJSON -> JSON
FormatBSON -> BSON
FormatTXT -> TXT
-}

newtype Hash = Hash String deriving (Generic, Eq)
instance Show Hash where show (Hash s) = s

newtype Chanel = Chanel String deriving (Generic, Eq)
instance ToJSON Chanel
instance FromJSON Chanel
instance Show Chanel where show (Chanel s) = s
instance Arbitrary Chanel where
    arbitrary = Chanel <$> arbitrary

newtype SourceId = SourceId String deriving (Generic, Eq)
instance ToJSON SourceId
instance FromJSON SourceId
instance Show SourceId where show (SourceId s) = s
instance Arbitrary SourceId where
    arbitrary = SourceId <$> arbitrary

newtype SessionId = SessionId String deriving (Generic, Eq)
instance ToJSON SessionId
instance FromJSON SessionId
instance Show SessionId where show (SessionId s) = s
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
    } deriving (Generic, Eq)

instance Arbitrary Event where
    arbitrary = Event
        <$> arbitrary
        <*> arbitrary
        <*> (UTCTime <$> day <*> diffT)
        <*> (TimeSpec <$> arbitrary <*> arbitrary)
        <*> arbitrary
        <*> (BS.pack <$> arbitrary)
      where
        day = fromGregorian <$> arbitrary <*> arbitrary <*> arbitrary
        diffT = secondsToDiffTime <$> arbitrary

instance Show Event where
    show e = "Event " ++ show
        [ show $ eChanel e
        , show $ eSourceId e
        , formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ" $ eUtcTime e
        , show $ toNanoSecs $ eBootTime e
        , show $ eSessionId e
        , hexlify $ eValue e
        ]

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

hexlify :: BS.ByteString -> String
hexlify = foldr (++) "" . map (printf "%02X") . BS.unpack

unhexlify :: String -> Maybe BS.ByteString
unhexlify s = do
    nums <- getPairs [] s >>= sequence . map getNum
    return $ BS.pack nums
  where
    getPairs acc [] = Just acc
    getPairs _ (_:[]) = Nothing
    getPairs acc (a:b:xs) = getPairs ([a,b]:acc) xs

    getNum x = case readHex x of
        [(a,"")] -> Just a
        _ -> Nothing

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

-- get current time (boot and UTC)
now :: IO (UTCTime, TimeSpec)
now = do
    -- TODO: check if possible to protect this section from being interrupted
    t1 <- getCurrentTime
    t2 <- getTime Boottime
    return (t1, t2)

