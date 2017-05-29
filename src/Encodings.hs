------------------
-- |
-- Module: Encodings
--
-- This module defines encoding/decding.
--

{-# LANGUAGE DeriveGeneric #-}

module Encodings
( JSONFormat(..)
, EncodeFormat(..)
, Encodable(..)
, encodeFormatOptions
, join, split
, encodeList, decodeList
, hexlify, unhexlify
, cobsEncode, cobsDecode
) where

import Control.Monad (guard)
import Data.Monoid ((<>))
import qualified Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Internal as BS (c2w)
import GHC.Generics (Generic)
import Options.Applicative ((<|>))
import qualified Options.Applicative as Opt
import qualified Test.QuickCheck as QC
import Text.Printf (printf)
import Numeric (readHex)

-- | JSON format variants
data JSONFormat
    = JSONCompact       -- one line
    | JSONPretty Int    -- multiple lines with specified indent size
    deriving (Generic, Eq, Show)

instance QC.Arbitrary JSONFormat where
    arbitrary = QC.oneof
        [ pure JSONCompact
        -- , JSONPretty <$> QC.choose (1,8)  -- TODO: decoding not implemented
        ]

-- | File encoding formats.
data EncodeFormat
    = EncText               -- text encoding, based on Show and Read
    | EncBin                -- binary encoding (without zero bytes)
    | EncJSON JSONFormat    -- JSON encoding
    deriving (Generic, Eq, Show)

instance QC.Arbitrary EncodeFormat where
    arbitrary = QC.oneof
        [ pure EncText
        , pure EncBin
        , EncJSON <$> QC.arbitrary
        ]

encodeFormatOptions :: Opt.Parser EncodeFormat
encodeFormatOptions =
    ( Opt.flag' EncText (Opt.long "text" <> Opt.help "\"text\" file encoding")
  <|> Opt.flag' EncBin (Opt.long "bin" <> Opt.help "\"binary\" file encoding")
  <|> Opt.flag' (EncJSON JSONCompact)
        (Opt.long "jsonCompact" <> Opt.help "Compact \"json\" file encoding")
  <|> (EncJSON <$> JSONPretty <$> Opt.option Opt.auto
        ( Opt.long "jsonPretty"
       <> Opt.metavar "N"
       <> Opt.help "Pretty \"json\" file encoding (N indent spaces)")
        )
    )

-- | An interface to encode and decode something to/from a bytestring.
class Encodable a where
    encode :: EncodeFormat -> a -> BSL.ByteString
    decode :: EncodeFormat -> BSL.ByteString -> Maybe a

-- | A delimiter for different encoding formats.
delim :: EncodeFormat -> Data.Word.Word8
delim fmt = case fmt of
    EncText -> BS.c2w '\n'
    EncBin -> 0
    EncJSON _ -> BS.c2w '\n'

-- | Join multiple bytestrings to a single bytestring.
-- Insert a dilimiter at the end of each bytestring, depending on encoding.
join :: EncodeFormat -> [BSL.ByteString] -> BSL.ByteString
join fmt lst = BSL.concat [i `mappend` BSL.singleton (delim fmt) | i <- lst]

-- | Split bytestring to multiple bytestrings, remove delimiters.
split :: EncodeFormat -> BSL.ByteString -> [BSL.ByteString]
split fmt s = case fmt of
    EncText -> parts
    EncBin -> parts
    EncJSON JSONCompact -> parts
    EncJSON (JSONPretty _) -> undefined -- TODO: not all '\n' are delimiters
  where
    parts = safeInit $ BSL.split (delim fmt) s
    safeInit [] = []
    safeInit lst = init lst

-- | Encode list of encodable items.
encodeList :: (Encodable a) => EncodeFormat -> [a] -> BSL.ByteString
encodeList fmt lst = join fmt (encode fmt <$> lst)

-- | Try to decode list of encodable items.
decodeList :: (Encodable a) => EncodeFormat -> BSL.ByteString -> Maybe [a]
decodeList fmt s = sequence (decode fmt <$> split fmt s)

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
cobsEncode :: BSL.ByteString -> BSL.ByteString
cobsEncode s = BSL.concat $ encodeSegments s where
    encodeSegments x = BSL.singleton prefix : chunk : rest where
        (a,b) = BSL.span (/= 0) x
        chunk = BSL.take 254 a
        n = BSL.length chunk
        prefix = fromIntegral $ succ n
        c = BSL.drop n a `BSL.append` b
        rest = case BSL.null c of
            True -> []
            False -> case prefix of
                255 -> encodeSegments c
                _ -> encodeSegments $ BSL.drop 1 c

-- | COBS decode single binary string.
cobsDecode :: BSL.ByteString -> Maybe BSL.ByteString
cobsDecode s = do
    (a,b) <- decodeSegment s
    case BSL.null b of
        True -> return a
        False -> do
            rest <- cobsDecode b
            return $ a `BSL.append` rest
  where
    decodeSegment :: BSL.ByteString -> Maybe (BSL.ByteString, BSL.ByteString)
    decodeSegment x = do
        (a,b) <- BSL.uncons x
        guard $ a > 0
        let n = pred $ fromIntegral a
            (c,d) = BSL.splitAt n b
        guard $ BSL.length c == n
        guard $ BSL.findIndex (== 0) c == Nothing
        case BSL.null d of
            True -> return (c, d)
            False -> case a of
                255 -> do
                    (e,f) <- decodeSegment d
                    return (c `BSL.append` e, f)
                _ -> return (c `BSL.append` BSL.singleton 0, d)

