------------------
-- |
-- Module: Encodings
--
-- This module defines encoding/decding.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Encodings
( EncodeFormat(..)
, HasSize(..)
, encode, decode
, jsonEncode, jsonEncodePretty, jsonDecode, jsonEitherDecode
, encodeFormatOptions
, encodeList, decodeList
, hexlify, unhexlify
, cobsEncode, cobsDecode
, toByteString, fromByteString
) where

import           Control.Monad
import           Data.Monoid ((<>))
import           Data.Word (Word8)

import qualified Data.Attoparsec.ByteString as ATP

import qualified Data.Aeson
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.Encode.Pretty as AP

import qualified Data.Serialize as Bin

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8

import           GHC.Generics (Generic)
import           Options.Applicative ((<|>))
import qualified Options.Applicative as Opt
import qualified Test.QuickCheck as QC
import           Text.Read (readMaybe)

import           Numeric (readHex)

-- local imports
import qualified Streams
import           Streams hiding (map)

class HasSize a where
    sizeOf :: a -> Integer

instance HasSize BS.ByteString where
    sizeOf = fromIntegral . BS.length

instance HasSize [a] where
    sizeOf = toInteger . length

-- | File encoding formats.
data EncodeFormat
    = EncText               -- text encoding, based on Show and Read
    | EncBin                -- binary encoding
    | EncJSON               -- JSON compact encoding
    | EncJSONPretty Int     -- JSON pretty encoding with ident spaces
    deriving (Generic, Eq, Show)

instance ToJSON EncodeFormat where
    toJSON EncText = toJSON ("text" :: String)
    toJSON EncBin = toJSON ("bin" :: String)
    toJSON EncJSON = toJSON ("json" :: String)
    toJSON (EncJSONPretty _n) = undefined
        -- toJSON (("jsonPretty " ++ show n) :: String)

instance FromJSON EncodeFormat where
    parseJSON (Data.Aeson.String s) = case s of
        "text" -> return EncText
        "bin" -> return EncBin
        "json" -> return EncJSON
        -- TODO: jsonPretty
        _ -> typeMismatch "EncodeFormat" (Data.Aeson.String s)
    parseJSON t = typeMismatch "EncodeFormat" t

instance QC.Arbitrary EncodeFormat where
    arbitrary = QC.oneof
        [ pure EncText
        , pure EncBin
        , pure EncJSON
        , EncJSONPretty <$> QC.choose (1,8)
        ]

encodeFormatOptions :: Opt.Parser EncodeFormat
encodeFormatOptions =
    ( Opt.flag' EncText (Opt.long "text" <> Opt.help "\"text\" file encoding")
  <|> Opt.flag' EncBin (Opt.long "bin" <> Opt.help "\"binary\" file encoding")
  <|> Opt.flag' EncJSON
        (Opt.long "json" <> Opt.help "Compact \"json\" file encoding")
  <|> (EncJSONPretty <$> Opt.option Opt.auto
        ( Opt.long "jsonPretty"
       <> Opt.metavar "N"
       <> Opt.help "Pretty \"json\" file encoding (N indent spaces)")
        )
    )

-- | Convert bytestring to hex representation.
hexlify :: BS.ByteString -> String
hexlify = concatMap f . BS.unpack where
    f val = byteAsHex !! (fromIntegral val)

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

-- | Encode to JSON.
jsonEncode :: (Data.Aeson.ToJSON a) => a -> BSL.ByteString
jsonEncode = Data.Aeson.encode

-- | Encode (pretty) to JSON.
jsonEncodePretty :: (Data.Aeson.ToJSON a) => a -> BSL.ByteString
jsonEncodePretty = AP.encodePretty'
    AP.defConfig {AP.confCompare = compare}

-- | Decode from JSON.
jsonDecode :: (Data.Aeson.FromJSON a) => BSL.ByteString -> Maybe a
jsonDecode = Data.Aeson.decode

-- | Try to decode from JSON.
jsonEitherDecode :: (Data.Aeson.FromJSON a) => BSL.ByteString -> Either String a
jsonEitherDecode = Data.Aeson.eitherDecode

-- | Encode item to a ByteString.
encode :: (Show a, Bin.Serialize a, Data.Aeson.ToJSON a) =>
    EncodeFormat -> a -> BS.ByteString
encode EncText val = BS.concat [BS8.pack $ show val, BS.singleton lineFeed]
encode EncBin val = BS.concat [cobsEncode $ Bin.encode val, BS.singleton 0]
encode EncJSON val = BS.concat
    [BSL.toStrict $ Data.Aeson.encode val, BS.singleton lineFeed]
encode (EncJSONPretty i) val = BS.concat
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
--  - check performance for very large files, for example
--    '\n\n...' without recordSeparator for jsonPretty
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
        EncJSON -> withEndDelim lineFeed Data.Aeson.decodeStrict
        EncJSONPretty _ -> ATP.parse jsonStream s
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
toByteString :: (Data.Aeson.ToJSON a, Bin.Serialize a, Show a) =>
    EncodeFormat -> Pipe a BS.ByteString c
toByteString fmt = Streams.map (encode fmt)

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
fromByteString :: (Data.Aeson.FromJSON a, Bin.Serialize a, Read a) =>
    Int
    -> EncodeFormat
    -> Pipe BS.ByteString (Either (String, BS.ByteString) a) c
fromByteString maxSize fmt = mkPipe $ go BS.empty where
    go acc consume produce = do
        let method = case BS.null acc of
                True -> Clear
                False -> MoreData
        s <- consume method
        let (lst, leftover) = decodeStream maxSize fmt (acc <> s)
        mapM_ produce lst
        go leftover consume produce

byteAsHex :: [String]
byteAsHex = ["00","01","02","03","04","05","06","07","08","09","0A","0B","0C","0D","0E","0F","10","11","12","13","14","15","16","17","18","19","1A","1B","1C","1D","1E","1F","20","21","22","23","24","25","26","27","28","29","2A","2B","2C","2D","2E","2F","30","31","32","33","34","35","36","37","38","39","3A","3B","3C","3D","3E","3F","40","41","42","43","44","45","46","47","48","49","4A","4B","4C","4D","4E","4F","50","51","52","53","54","55","56","57","58","59","5A","5B","5C","5D","5E","5F","60","61","62","63","64","65","66","67","68","69","6A","6B","6C","6D","6E","6F","70","71","72","73","74","75","76","77","78","79","7A","7B","7C","7D","7E","7F","80","81","82","83","84","85","86","87","88","89","8A","8B","8C","8D","8E","8F","90","91","92","93","94","95","96","97","98","99","9A","9B","9C","9D","9E","9F","A0","A1","A2","A3","A4","A5","A6","A7","A8","A9","AA","AB","AC","AD","AE","AF","B0","B1","B2","B3","B4","B5","B6","B7","B8","B9","BA","BB","BC","BD","BE","BF","C0","C1","C2","C3","C4","C5","C6","C7","C8","C9","CA","CB","CC","CD","CE","CF","D0","D1","D2","D3","D4","D5","D6","D7","D8","D9","DA","DB","DC","DD","DE","DF","E0","E1","E2","E3","E4","E5","E6","E7","E8","E9","EA","EB","EC","ED","EE","EF","F0","F1","F2","F3","F4","F5","F6","F7","F8","F9","FA","FB","FC","FD","FE","FF"]

