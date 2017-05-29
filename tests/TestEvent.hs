
module TestEvent (testEvent) where

import qualified Data.Aeson
import qualified Data.Binary
import Data.Time.Clock
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, (===))

import Event
import Encodings

testEvent :: Test
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
propBIN e = Data.Binary.decode (Data.Binary.encode e) == e

-- | encode and decode shall be inverse operations.
propEncodeDecode :: EncodeFormat -> Event -> Bool
propEncodeDecode fmt e = decode fmt (encode fmt e) == Just e

-- | encodeEvents and decodeEvents shall be inverse operations.
propEncodeDecodeMulti :: EncodeFormat -> [Event]-> Bool
propEncodeDecodeMulti fmt lst =
    decodeList fmt (encodeList fmt lst) == Just lst

-- | test leap seconds
propLeapEncodeDecode :: EncodeFormat -> Event -> Property
propLeapEncodeDecode fmt e' = prop where
    e = e' {eUtcTime = t}
    t = UTCTime
            (utctDay $ eUtcTime e')
            (picosecondsToDiffTime (24*3600*(10^(12::Int))-1))
    prop = decode fmt (encode fmt e) === Just e

