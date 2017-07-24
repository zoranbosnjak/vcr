
module TestEncodings (testEncodings) where

import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)
import Test.QuickCheck
    ( Gen, Property, forAll, resize, arbitrary, Positive(Positive)
    , suchThat, Arbitrary)

import Encodings

-- large bytestring generator
largeByteString :: (Arbitrary a) => Int -> Gen a
largeByteString n = resize n arbitrary

testEncodings :: Test
testEncodings = testGroup "Encodings"
    [ testGroup "hexlify"
        [ testProperty "hexlify" propHexlify
        ]
    , testGroup "COBS"
        [ testCase "examples" cobsExamples
        , testProperty "nonzero" propCobsEncodeNonZero
        , testProperty "length" propCobsEncodeLength
        , testProperty "inverse" propCobs
        ]
    ]

-- | hexlify and unhexlify shall be inverse operations
propHexlify :: Positive Int -> Property
propHexlify (Positive n) = forAll (BS.pack <$> largeByteString n) $ \bs -> do
    unhexlify (hexlify bs) == Just bs

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
            (hexlify $ BS.pack $ BS.unpack $ cobsEncode $ BS.pack x)
        assertEqual "inverse"
            (fmap hexlify $ Just $ BS.pack x)
            (fmap hexlify $ cobsDecode $ cobsEncode $ BS.pack x)

-- | COBS encoded string shall not contain any zero byte
propCobsEncodeNonZero :: (Positive Int) -> Property
propCobsEncodeNonZero (Positive n) = forAll sample $ \bs -> do
    BS.elemIndex 0 (cobsEncode bs) == Nothing
  where
    sample =
        (BS.pack <$> largeByteString n)
        `suchThat` (\s -> isJust (BS.elemIndex 0 s))

-- | COBS encoded string has at most one extra byte for each 254 data bytes
propCobsEncodeLength :: Property
propCobsEncodeLength = forAll (BS.pack <$> largeByteString 2000) $ \bs1 -> do
    let n = BS.length bs1
        bs2 = cobsEncode bs1
        overhead
            | n == 0 = 0
            | otherwise = div n 254
    BS.length bs2 > n && BS.length bs2 <= n + overhead + 1

-- | cobsEncode and cobsDecode shall be inverse operations
propCobs :: Property
propCobs = forAll (BS.pack <$> largeByteString 2000) $ \bs -> do
    cobsDecode (cobsEncode bs) == Just bs

