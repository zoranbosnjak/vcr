{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Event where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.Time
import GHC.Generics
import System.Clock
import Text.Printf
import Numeric (readHex)

newtype Hash = Hash String deriving (Generic, Eq)

newtype Chanel = Chanel String deriving (Generic, Eq)
instance ToJSON Chanel
instance FromJSON Chanel
instance Show Chanel where show (Chanel s) = show s

newtype SourceId = SourceId String deriving (Generic, Eq)
instance ToJSON SourceId
instance FromJSON SourceId
instance Show SourceId where show (SourceId s) = show s

newtype SessionId = SessionId String deriving (Generic, Eq)
instance ToJSON SessionId
instance FromJSON SessionId
instance Show SessionId where show (SessionId s) = show s

data Event = Event
    { eChanel   :: Chanel
    , eSourceId :: SourceId     -- id of the recorder
    , eUtcTime  :: UTCTime      -- capture utc time
    , eMonotonicTime :: TimeSpec   -- capture monotonic time
    , eArchive  :: Bool         -- do not auto delete this event
    , eSessionId :: SessionId   -- monotonic time is valid only within
                                -- the same session ID
    , eValue    :: BS.ByteString   -- the event value
    } deriving (Generic, Eq)

instance Show Event where
    show e = foldr1 (\a b -> a ++ " " ++ b) $
        [ "Event"
        , show $ eChanel e
        , show $ eSourceId e
        , show $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ" $
            eUtcTime e
        , show $ toNanoSecs $ eMonotonicTime e
        , show $ eArchive e
        , show $ eSessionId e
        , show $ hexlify $ eValue e
        ]

instance ToJSON Event where
    toJSON (Event ch src utcTime monoTime arc ses val) = object
        [ "ch" .= ch
        , "src" .= src
        , "utc" .= utcTime
        , "mono" .= toNanoSecs monoTime
        , "archive" .= arc
        , "session" .= ses
        , "value" .= hexlify val
        ]

instance FromJSON Event where
    parseJSON (Object v) = Event <$>
        v .: "ch" <*>
        v .: "src" <*>
        v .: "utc" <*>
        fmap fromNanoSecs (v .: "mono") <*>
        v .: "archive" <*>
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
hash = undefined

-- get current time (monotonic and UTC)
now :: IO (UTCTime, TimeSpec)
now = do
    -- TODO: check if possible to protect this section from being interrupted
    t1 <- getCurrentTime
    t2 <- getTime Monotonic
    return (t1, t2)

