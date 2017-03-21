
module TestEvent (testEvent) where

import qualified Data.Aeson
import qualified Data.Binary
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.API
    ( TestOptions'(TestOptions)
    , plusTestOptions
    , topt_seed, topt_maximum_generated_tests
    , topt_maximum_unsuitable_generated_tests
    , topt_maximum_test_size
    , topt_maximum_test_depth
    , topt_timeout
    )
import Test.HUnit (Assertion, assertEqual)
import Test.QuickCheck
    ( Gen, Property, forAll, resize, arbitrary, Positive(Positive)
    , suchThat)

import Event

nTimes :: Test -> Int -> Test
nTimes t n = plusTestOptions opts t where
    opts = TestOptions
        { topt_seed = Nothing
        , topt_maximum_generated_tests = Just n
        , topt_maximum_unsuitable_generated_tests = Just (n * 1000)
        , topt_maximum_test_size = Nothing
        , topt_maximum_test_depth = Nothing
        , topt_timeout = Nothing
        }

-- large bytestring generator
largeByteString :: Int -> Gen BS.ByteString
largeByteString n = BS.pack <$> resize n arbitrary

testEvent :: Test
testEvent = testGroup "Event"
    [ testGroup "hexlify"
        [ testProperty "hexlify" propHexlify            `nTimes` 10000
        ]
    , testGroup "JSON"
        [ testProperty "json" propJSON                  `nTimes` 10000
        ]
    , testGroup "BIN"
        [ testProperty "bin" propBIN                    `nTimes` 10000
        ]
    , testGroup "COBS"
        [ testCase "examples" cobsExamples
        , testProperty "nonzero" propCobsEncodeNonZero  `nTimes` 10000
        , testProperty "length" propCobsEncodeLength    `nTimes` 10000
        , testProperty "inverse" propCobs               `nTimes` 10000
        ]
    , testGroup "encode/decode"
        [ testProperty "single" propEncodeDecode        `nTimes` 10000
        , testProperty "multiple" propEncodeDecodeMulti `nTimes` 10000
        ]
    ]

-- | hexlify and unhexlify shall be inverse operations
propHexlify :: Positive Int -> Property
propHexlify (Positive n) = forAll (largeByteString n) $ \bs -> do
    unhexlify (hexlify bs) == Just bs

-- | JSON encode/decode gets the same event
propJSON :: Event -> Bool
propJSON e = Data.Aeson.decode (Data.Aeson.encode e) == Just e

-- | Bin encode/decode gets the same event
propBIN :: Event -> Bool
propBIN e = Data.Binary.decode (Data.Binary.encode e) == e

-- | COBS encode test examples
cobsExamples :: Assertion
cobsExamples = do
    -- basic examples
    f [0x00]                [0x01,0x01]
    f [0x00,0x00]           [0x01,0x01,0x01]
    f [0x11,0x22,0x00,0x33] [0x03,0x11,0x22,0x02,0x33]
    f [0x11,0x22,0x33,0x44] [0x05,0x11,0x22,0x33,0x44]
    f [0x11,0x00,0x00,0x00] [0x02,0x11,0x01,0x01,0x01]
    f [0x01,0x02..0xFE]     $ [0xFF] ++ [0x01,0x02..0xFE]
    -- more examples
    f []                    [0x01]
    f [0x01]                [0x02,0x01]
    f [1..253]              $ [0xFE] ++ [1..253]
    f [1..254]              $ [0xFF] ++ [1..254]
    f [1..255]              $ [0xFF] ++ [1..254] ++ [0x02,0xFF]
    f [0..255]              $ [0x01,0xFF] ++ [1..254] ++ [0x02,0xFF]
  where
    f x y = do
        assertEqual "differ"
            (hexlify $ BS.pack y)
            (hexlify $ cobsEncode $ BS.pack x)
        assertEqual "inverse"
            (fmap hexlify $ Just $ BS.pack x)
            (fmap hexlify $ cobsDecode $ cobsEncode $ BS.pack x)

-- | COBS encoded string shall not contain any zero byte
propCobsEncodeNonZero :: (Positive Int) -> Property
propCobsEncodeNonZero (Positive n) = forAll sample $ \bs -> do
    BS.elemIndex 0 (cobsEncode bs) == Nothing
  where
    sample = largeByteString n `suchThat` (\s -> isJust (BS.elemIndex 0 s))

-- | COBS encoded string has at most one extra byte for each 254 data bytes
propCobsEncodeLength :: Property
propCobsEncodeLength = forAll (largeByteString 2000) $ \bs1 -> do
    let n = BS.length bs1
        bs2 = cobsEncode bs1
        overhead
            | n == 0 = 0
            | otherwise = div n 254
    BS.length bs2 > n && BS.length bs2 <= n + overhead + 1

-- | cobsEncode and cobsDecode shall be inverse operations
propCobs :: Property
propCobs = forAll (largeByteString 2000) $ \bs -> do
    cobsDecode (cobsEncode bs) == Just bs

-- | encodeEvent and decodeEvent shall be inverse operations.
propEncodeDecode :: EncodeFormat -> Event -> Bool
propEncodeDecode fmt e = decodeEvent fmt (encodeEvent fmt e) == Just e

-- | encodeEvents and decodeEvents shall be inverse operations.
propEncodeDecodeMulti :: EncodeFormat -> [Event]-> Bool
propEncodeDecodeMulti fmt lst =
    decodeEvents fmt (encodeEvents fmt lst) == Just lst

