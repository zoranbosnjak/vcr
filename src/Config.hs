
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}

module Config where

-- standard imports
import           GHC.Generics (Generic)
import           Control.Monad
import           Control.Applicative
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Text as Text
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- local imports
import Types

data ConfUdp = ConfUdp
    { uMcast    :: Maybe Mcast
    , uPort     :: Maybe Port
    , uIp       :: Maybe Ip
    } deriving (Generic, Eq, Show)

instance ToJSON ConfUdp where
    toJSON (ConfUdp a b c) = object
        [ "mcast"   .= a
        , "port"    .= b
        , "ip"      .= c
        ]

instance FromJSON ConfUdp where
    parseJSON = withObject "ConfUdp" $ \v -> ConfUdp
        <$> v .: "mcast"
        <*> v .: "port"
        <*> v .: "ip"

data ConfHosts
    = HostAll
    | HostSet (Set.Set Host)
    | HostMap (Map.Map Host ConfUdp)
    deriving (Generic, Eq, Show)

instance ToJSON ConfHosts where
    toJSON HostAll = toJSON ("all" :: Text)
    toJSON (HostSet val) = toJSON val
    toJSON (HostMap val) = toJSON val

instance FromJSON ConfHosts where
    parseJSON (String val) = case val of
        "all" -> pure HostAll
        _ -> typeMismatch "ConfHosts" $ String "expecting \"all\""
    parseJSON (Array val) = HostSet <$> parseJSON (Array val)
    parseJSON (Object val) = HostMap <$> parseJSON (Object val)
    parseJSON invalid = typeMismatch "ConfHosts" invalid

data ConfInput = ConfInput
    { inMcast :: Maybe Mcast
    , inPort  :: Maybe Port
    , inIp    :: Maybe Ip
    , inHosts :: ConfHosts
    } deriving (Generic, Eq, Show)

instance ToJSON ConfInput where
    toJSON (ConfInput a b c d) = object
        [ "mcast"   .= a
        , "port"    .= b
        , "ip"      .= c
        , "hosts"   .= d
        ]

instance FromJSON ConfInput where
    parseJSON = withObject "ConfInput" $ \v -> ConfInput
        <$> v .: "mcast"
        <*> v .: "port"
        <*> v .: "ip"
        <*> v .: "hosts"

data ConfInputs = ConfInputs (Map.Map Channel ConfInput)
    deriving (Generic, Eq, Show)

instance ToJSON ConfInputs
instance FromJSON ConfInputs

-- | Convert 'global config input' to input configuration for particular host.
inputConfig :: SourceId -> BS.ByteString -> Either String Inputs
inputConfig self raw = do
    val <- case eitherDecode (BSL.fromStrict raw) of
        Left e -> Left e
        Right cfg -> return (cfg :: Map.Map Channel ConfInput)
    return $ Inputs $ Map.mapMaybe verify val
  where
    verify conf = do
        let mcast1 = inMcast conf
            port1 = inPort conf
            ip1 = inIp conf
            cfg = ConfUdp mcast1 port1 ip1
        hostParams <- case inHosts conf of
            HostAll -> return cfg
            HostSet s -> (guard $ self `elem` s) >> return cfg
            HostMap m -> Map.lookup self m
        let mcast   = uMcast hostParams <|> mcast1
            port    = uPort hostParams <|> port1
            ip      = uIp hostParams <|> ip1
        Just $ do
            port' <- maybe (Left "'port' not defined") return port
            case mcast of
                Just val -> return $ UdpInMulticast val port' ip
                Nothing -> do
                    let msg = "'ip' nor 'mcast' defined"
                    ip' <- maybe (Left msg) return ip
                    return $ UdpInUnicast ip' port'

