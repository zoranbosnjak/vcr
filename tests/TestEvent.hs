
module TestEvent (testEvent) where

import qualified Data.Aeson
import qualified Data.Serialize
import Data.Time.Clock

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Event
import Encodings

testEvent :: TestTree
testEvent = testGroup "Event"
    [ testGroup "JSON"
        [ testProperty "json" propJSON
        ]
    , testGroup "BIN"
        [ testProperty "bin" propBIN
        ]
    , testGroup "encode/decode"
        [ testProperty "single" propEncodeDecode
        , testProperty "multiple" propEncodeDecodeMulti
        ]
    , testGroup "leap second"
        [ testProperty "encode/decode" propLeapEncodeDecode
        ]
    ]

-- | JSON encode/decode gets the same event
propJSON :: Event -> Bool
propJSON e = Data.Aeson.decode (Data.Aeson.encode e) == Just e

-- | Bin encode/decode gets the same event
propBIN :: Event -> Bool
propBIN e = Data.Serialize.decode (Data.Serialize.encode e) == Right e

-- | encode and decode shall be inverse operations.
propEncodeDecode :: EncodeFormat -> Event -> Bool
propEncodeDecode fmt e = tryDecode fmt (encode fmt e) == Just e

-- | encodeEvents and decodeEvents shall be inverse operations.
propEncodeDecodeMulti :: EncodeFormat -> [Event]-> Bool
propEncodeDecodeMulti fmt lst =
    tryDecodeList fmt (encodeList fmt lst) == Just lst

-- | test leap seconds
propLeapEncodeDecode :: EncodeFormat -> Event -> Property
propLeapEncodeDecode fmt e' = prop where
    e = e' {eUtcTime = UtcTime t}
    UtcTime u = eUtcTime e'
    t = UTCTime
            (utctDay u)
            (picosecondsToDiffTime (24*3600*(10^(12::Int))-1))
    prop = tryDecode fmt (encode fmt e) === Just e

