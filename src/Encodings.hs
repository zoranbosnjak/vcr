------------------
-- |
-- Module: Encodings
--
-- This module defines encoding/decding.
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Encodings
( EncodeFormat(..)
, HasSize(..)
, encode, tryDecode
, jsonEncode, jsonEncodePretty, jsonDecode, jsonEitherDecode
, encodeFormatOptions
, encodeList, tryDecodeList
, hexlify, unhexlify
, cobsEncode, cobsDecode
, toByteString, fromByteString
)
where

import           Control.Monad
import           Control.Concurrent.STM
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
import qualified Data.ByteString.Base16 as B16

import           GHC.Generics (Generic)
import           Options.Applicative ((<|>))
import qualified Options.Applicative as Opt
import qualified Test.QuickCheck as QC
import           Text.Read (readMaybe)
import qualified Data.Text as Text

import           Streams

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
    toJSON (EncJSONPretty n) = toJSON (("jsonPretty " ++ show n))

instance FromJSON EncodeFormat where
    parseJSON (Data.Aeson.String s) = case Text.words s of
        ["text"] -> return EncText
        ["bin"] -> return EncBin
        ["json"] -> return EncJSON
        ["jsonPretty", n] -> return $ EncJSONPretty (read $ Text.unpack n)
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
hexlify = BS8.unpack . B16.encode

-- | Convert hex representation back to a bytestring.
unhexlify :: String -> Maybe BS.ByteString
unhexlify st = do
    let (a,b) = B16.decode $ BS8.pack st
    guard $ BS.null b
    return a

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

-- | Encode list of encodable items.
encodeList :: (Data.Aeson.ToJSON a, Bin.Serialize a, Show a) =>
    EncodeFormat -> [a] -> BS.ByteString
encodeList fmt lst = BS.concat (encode fmt <$> lst)

-- | Pipe from items to bytestrings.
toByteString :: (Data.Aeson.ToJSON a, Bin.Serialize a, Show a) =>
    EncodeFormat -> Pipe a BS.ByteString c
toByteString fmt = Streams.map (encode fmt)

parser :: (Read b, Bin.Serialize b, FromJSON b) => EncodeFormat -> ATP.Parser b
parser = \case
    EncText -> do
        val <- ATP.takeWhile1 (/= lineFeed) <* ATP.word8 lineFeed
        maybe (fail "decode error") return $
            readMaybe $ BS8.unpack val
    EncBin -> do
        mVal <- ATP.takeWhile1 (/= 0) <* ATP.word8 0
        case cobsDecode mVal of
            Nothing -> fail "COBS decode error"
            Just val -> either fail return (Bin.decode val)
    EncJSON -> do
        val <- ATP.takeWhile1 (/= lineFeed) <* ATP.word8 lineFeed
        maybe (fail "decode error") return $ Data.Aeson.decodeStrict val
    EncJSONPretty _ -> do
        val <-
            ATP.word8 recordSeparator
            *> Data.Aeson.json
            <* ATP.word8 lineFeed
        case Data.Aeson.fromJSON val of
            Data.Aeson.Error e -> fail e
            Data.Aeson.Success a -> return a

-- | Try to decode single item.
tryDecode :: (Read a, Bin.Serialize a, Data.Aeson.FromJSON a) =>
    EncodeFormat -> BS.ByteString -> Maybe a
tryDecode fmt s = either (const Nothing) return $ ATP.parseOnly (parser fmt) s

-- | Try to decode list of encodable items.
tryDecodeList :: (Data.Aeson.FromJSON a, Bin.Serialize a, Read a) =>
    EncodeFormat -> BS.ByteString -> Maybe [a]
tryDecodeList fmt s = loop s where
    loop leftover
        | leftover == mempty = Just []
        | otherwise = case ATP.parse (parser fmt) leftover of
            ATP.Done i r -> do
                rest <- loop i
                Just (r:rest)
            _ -> Nothing

-- | Pipe from ByteString to Either (error, item).
fromByteString :: (Data.Aeson.FromJSON a, Bin.Serialize a, Read a) =>
    Int
    -> EncodeFormat
    -> Pipe BS.ByteString (Either (String, BS.ByteString) a) c
fromByteString maxSize fmt = mkPipe (loop BS.empty)
  where
    loop acc consume produce = atomically consume >>= \case
        EndOfData rv
            | BS.null acc -> return rv          -- clean exit
            | otherwise -> fail "broken pipe"   -- more data is expected
        Message s -> do
            leftover <- process produce (acc <> s)
            case BS.length leftover >= maxSize of
                True -> fail "input data too long"
                False -> loop leftover consume produce

    process produce s = case ATP.parse (parser fmt) s of
        ATP.Fail i _c e -> do
            _ <- atomically $ produce $ Left (e,s)
            process produce i
        ATP.Partial _f -> return s
        ATP.Done i r -> do
            _ <- atomically $ produce $ Right r
            process produce i

