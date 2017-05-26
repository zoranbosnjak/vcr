------------------
-- |
-- Module: Encodings
--
-- This module defines encoding/decding.
--

{-# LANGUAGE DeriveGeneric #-}

module Encodings
( Encodable(..)
, EncodeFormat(..)
, JSONFormat(..)
, encodeFormatOptions
, delimit
, hexlify, unhexlify
, cobsEncode, cobsDecode
) where

import Control.Monad (guard)
import Data.Monoid ((<>))
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import GHC.Generics (Generic)
import Numeric (readHex)
import Options.Applicative ((<|>))
import qualified Options.Applicative as Opt
import qualified Test.QuickCheck as QC
import Text.Printf (printf)
import Data.Maybe

-- | Interface for encode and decode something.
class Encodable a where
    encode :: EncodeFormat -> a -> BS.ByteString
    decode :: EncodeFormat -> BS.ByteString -> Maybe a

    -- Encode list of items with default implementation
    encodeList :: EncodeFormat -> [a] -> BS.ByteString
    encodeList fmt lst = foldr mappend BS.empty
        [encode fmt e `mappend` pack (delimit fmt) | e <- lst]

    -- Try to decode list of items with default implementation.
    decodeList :: EncodeFormat -> BS.ByteString -> Maybe [a]
    decodeList fmt s
        | BS.null s = Just []
        | otherwise = do
            (e,s') <- tryDecode s
            rest <- decodeList fmt s'
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
            firstOf [(decode fmt a,b) | (a,b) <- probes]

    -- TODO: docs
    decodeFirst :: EncodeFormat -> BS.ByteString ->
                   (Maybe a, BS.ByteString)
    decodeFirst fmt str =
        case filter (isJust.fst)
                    [(decode fmt pfx,rest)
                     |(pfx,rest) <- (splits BS.empty str)] of
          []       -> (Nothing, str)
          (just:_) -> just
      where
        bsDelimiter :: BS.ByteString
        bsDelimiter = pack $ delimit fmt

        splits :: BS.ByteString -> BS.ByteString
               -> [(BS.ByteString, BS.ByteString)]
        splits pfx rest
          | BS.null rest' = []
          | otherwise     = (newPfx,newRest):(splits newPfx newRest)
          where
            (pfx',rest') = BS.breakSubstring bsDelimiter rest
            newPfx = BS.append pfx pfx'
            newRest = BS.drop (BS.length bsDelimiter) rest'

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
    = EncText
    | EncBin
    | EncJSON JSONFormat
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

-- | Event delimiter for different encoding formats.
delimit :: EncodeFormat -> String
delimit EncText = "\n"
delimit EncBin = "\0"
delimit (EncJSON _) = "\n"

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

