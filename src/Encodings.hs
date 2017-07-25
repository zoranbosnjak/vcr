------------------
-- |
-- Module: Encodings
--
-- This module defines encoding/decding.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Encodings
{-
( JSONFormat(..)
, EncodeFormat(..)
, Encodable(..)
, encodeFormatOptions
, join, split
, encodeList, decodeList
, hexlify, unhexlify
, cobsEncode, cobsDecode
) -} where

import           Control.Monad
import           Data.Monoid ((<>))
import           Data.Word (Word8)

import           Pipes
import qualified Pipes.Prelude as P

import qualified Data.Attoparsec.ByteString as ATP

import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AP

import qualified Data.Serialize as Bin

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8

import           GHC.Generics (Generic)
import           Options.Applicative ((<|>))
import qualified Options.Applicative as Opt
import qualified Test.QuickCheck as QC
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Numeric (readHex)

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
    = EncText               -- text encoding, based on Show and Read
    | EncBin                -- binary encoding
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
        c = BS.drop n a <> b
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
            return $ a <> rest
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
                    return (c <> e, f)
                _ -> return (c <> BS.singleton 0, d)

recordSeparator :: Word8
recordSeparator = 0x1E

lineFeed :: Word8
lineFeed = 0x0A

-- | Encode item to a ByteString.
encode :: (Show a, Bin.Serialize a, Data.Aeson.ToJSON a) =>
    EncodeFormat -> a -> BS.ByteString
encode EncText val = BS.concat [BS8.pack $ show val, BS.singleton lineFeed]
encode EncBin val = BS.concat [cobsEncode $ Bin.encode val, BS.singleton 0]
encode (EncJSON JSONCompact) val = BS.concat
    [BSL.toStrict $ Data.Aeson.encode val, BS.singleton lineFeed]
encode (EncJSON (JSONPretty i)) val = BS.concat
    [ BS.singleton recordSeparator
    , BSL.toStrict $ AP.encodePretty' cfg val
    , BS.singleton lineFeed
    ]
  where
    cfg = (AP.defConfig {AP.confCompare = compare, AP.confIndent = AP.Spaces i})

-- | Item parser.
-- TODO:
--  - change this function to use proper parsers
--    for all formats (like jsonStream)
--  - compare performance (BS.elem... vs parser)
--  - BS.null check might not be necessary any more
parse :: (Read a, Bin.Serialize a, Data.Aeson.FromJSON a) =>
    Int -> EncodeFormat -> BS.ByteString -> ATP.Result a
parse maxSize fmt s
    | BS.null s = ATP.Partial (parse maxSize fmt)
    | BS.length s > maxSize = ATP.Fail BS.empty [] "Input data too long."
    | otherwise = case fmt of
        EncText -> withEndDelim lineFeed (readMaybe . BS8.unpack)
        EncBin -> withEndDelim 0 $ \x -> do
            val <- cobsDecode x
            case Bin.decode val of
                Left _e -> Nothing
                Right a -> Just a
        EncJSON JSONCompact -> withEndDelim lineFeed Data.Aeson.decodeStrict
        EncJSON (JSONPretty _) -> ATP.parse jsonStream s
  where
    withEndDelim delim tryParse = case BS.elemIndex delim s of
            Nothing -> ATP.Partial (\more -> parse maxSize fmt (s <> more))
            Just i ->
                let (probe, rest) = BS.splitAt i s
                    leftover = BS.drop 1 rest
                in case tryParse probe of
                    Nothing -> ATP.Fail leftover [] "Decode error."
                    Just a -> ATP.Done leftover a

    jsonStream = do
        _ <- ATP.word8 recordSeparator
        val <- Data.Aeson.json
        _ <- ATP.word8 lineFeed
        case Data.Aeson.fromJSON val of
            Data.Aeson.Error e -> fail e
            Data.Aeson.Success a -> return a

-- | Encode list of encodable items.
encodeList :: (Data.Aeson.ToJSON a, Bin.Serialize a, Show a) =>
    EncodeFormat -> [a] -> BS.ByteString
encodeList fmt lst = BS.concat (encode fmt <$> lst)

-- | Pipe from items to bytestrings.
toByteString :: (Monad m, Data.Aeson.ToJSON a, Bin.Serialize a, Show a) =>
    EncodeFormat -> Pipe a BS.ByteString m ()
toByteString fmt = P.map (encode fmt)

-- | Decode ByteString to list of items + some remaining.
decodeStream :: (Read a, Bin.Serialize a, Data.Aeson.FromJSON a) =>
    Int
    -> EncodeFormat
    -> BS.ByteString
    -> ([Either (String, BS.ByteString) a], BS.ByteString)
decodeStream maxSize fmt orig = go [] orig where
    go acc s = case parse maxSize fmt s of
        ATP.Fail i _c e -> go (Left (e,s) : acc) i
        ATP.Partial _f -> (reverse acc, s)
        ATP.Done i r -> go (Right r : acc) i

-- | Try to decode list of encodable items.
decodeList :: (Data.Aeson.FromJSON a, Bin.Serialize a, Read a) =>
    Int -> EncodeFormat -> BS.ByteString -> Maybe [a]
decodeList maxSize fmt s = do
    guard $ BS.null leftover
    sequence (check <$> lst)
  where
    (lst, leftover) = decodeStream maxSize fmt s
    check (Left _) = Nothing
    check (Right val) = Just val

-- | Try to decode single item.
decode :: (Read a, Bin.Serialize a, Data.Aeson.FromJSON a) =>
    EncodeFormat -> BS.ByteString -> Maybe a
decode fmt s = do
    result <- decodeList maxBound fmt s
    case result of
        [a] -> Just a
        _ -> Nothing

-- | Pipe from ByteString to item.
fromByteString :: (Monad m, Data.Aeson.FromJSON a, Bin.Serialize a, Read a) =>
    Int
    -> EncodeFormat
    -> Pipe BS.ByteString (Either (String, BS.ByteString) a) m ()
fromByteString maxSize fmt = go BS.empty where
    go acc = do
        s <- await
        let (lst, leftover) = decodeStream maxSize fmt (acc <> s)
        mapM_ yield lst
        go leftover

