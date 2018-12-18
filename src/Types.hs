
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           GHC.Generics (Generic)
import           Data.Text as Text
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.Map as Map

type Host = Text
type Mcast = Text
type Ip = Text
type Port = Text
type TTL = Int
type ZkEndpoint = String

skipWords :: Int -> String -> String
skipWords n y
    | n <= 0 = y
    | otherwise = skipWords (pred n) $
        (Prelude.dropWhile (/= ' ') . Prelude.dropWhile (== ' ') $ y)

data UdpIn
    = UdpInUnicast Ip Port
    | UdpInMulticast Mcast Port (Maybe Ip)
    deriving (Generic, Eq)

instance Show UdpIn where
    show (UdpInUnicast ip port) =
        "Unicast " ++ Text.unpack ip ++ " " ++ Text.unpack port
    show (UdpInMulticast mcast port Nothing) =
        "Multicast " ++ Text.unpack mcast ++ " " ++ Text.unpack port
    show (UdpInMulticast mcast port (Just loc)) =
        "Multicast " ++ Text.unpack mcast ++ " " ++ Text.unpack port ++ " "
            ++ Text.unpack loc

{-
instance Read UdpIn where
    readsPrec _ s = case words s of
        ("Unicast":ip:port:_) -> [(UdpIn ip port Nothing, skipWords 3 s)]
        ("Multicast":mcast:port:localIp:_) ->
            [(UdpIn localIp port (Just mcast), skipWords 4 s)]
        _ -> []
-}

instance ToJSON UdpIn where
    toJSON (UdpInUnicast ip port) = toJSON $ Text.concat
        ["Unicast ", ip, " ", port]
    toJSON (UdpInMulticast mcast port Nothing) = toJSON $ Text.concat
        ["Multicast ", mcast, " ", port]
    toJSON (UdpInMulticast mcast port (Just loc)) = toJSON $ Text.concat
        ["Multicast ", mcast, " ", port, " ", loc]

instance FromJSON UdpIn where
    parseJSON (Data.Aeson.String s) = case (Text.words s) of
        ("Unicast":ip:port:[]) ->
            return $ UdpInUnicast ip port
        ("Multicast":mcast:port:[]) ->
            return $ UdpInMulticast mcast port Nothing
        ("Multicast":mcast:port:loc:[]) ->
            return $ UdpInMulticast mcast port (Just loc)
        _ -> typeMismatch "UdpIn" (Data.Aeson.String s)
    parseJSON t = typeMismatch "UdpIn" t

type Channel = String
type SourceId = Text
type SessionId = Text
type TrackId = Text

newtype Inputs = Inputs (Map.Map Channel (Either String UdpIn))
    deriving (Generic, Eq, Show, Semigroup, Monoid)

instance ToJSON Inputs
instance FromJSON Inputs

