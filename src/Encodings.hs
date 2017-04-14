------------------
-- |
-- Module: Encodings
--
-- This module defines encoding/decding.
--

{-# LANGUAGE DeriveGeneric #-}

module Encodings
-- (
-- TODO: add explicit exports of this module
{-)-} where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Numeric (readHex)
import qualified Test.QuickCheck as QC
import Text.Printf (printf)

-- | JSON format variants
data JSONFormat
    = JSONCompact       -- one line
    | JSONPretty Int    -- multiple lines with specified indent size
    deriving (Generic, Eq, Show)

instance QC.Arbitrary JSONFormat where
    arbitrary = QC.oneof
        [ pure JSONCompact
        , JSONPretty <$> QC.choose (1,8)
        ]

-- | File encoding formats.
data EncodeFormat
    = EncShow
    | EncJSON JSONFormat
    | EncBin
    deriving (Generic, Eq, Show)

instance QC.Arbitrary EncodeFormat where
    arbitrary = QC.oneof
        [ pure EncShow
        , EncJSON <$> QC.arbitrary
        , pure EncBin
        ]

-- | Event delimiter for different encoding formats.
delimit :: EncodeFormat -> String
delimit EncShow = "\n"
delimit (EncJSON _) = "\n"
delimit EncBin = "\0"

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

