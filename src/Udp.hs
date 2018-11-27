------------------
-- |
-- Module: Udp
--
-- This module provides common UDP definitions.
--

{-# LANGUAGE DeriveGeneric #-}

module Udp where

import           Control.Monad
import qualified Data.Aeson
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NB
import qualified Network.Multicast as Mcast
import qualified Options.Applicative as Opt
import           GHC.Generics (Generic)
import           Data.Text (unpack)

import           Pipes
import           Pipes.Safe

type Ip = String
type Port = String
type Mcast = String
type TTL = Int

skipWords :: Int -> String -> String
skipWords n y
    | n <= 0 = y
    | otherwise = skipWords (pred n) $
        (dropWhile (/= ' ') . dropWhile (== ' ') $ y)

-- | UDP (unicast or multicast)

data UdpIn = UdpIn Ip Port (Maybe Mcast) deriving (Generic, Eq)

instance Show UdpIn where
    show (UdpIn ip port Nothing) = "Unicast " ++ ip ++ " " ++ port
    show (UdpIn localIp port (Just mcast)) =
        "Multicast " ++ mcast ++ " " ++ port ++ " " ++ localIp

instance Read UdpIn where
    readsPrec _ s = case words s of
        ("Unicast":ip:port:_) -> [(UdpIn ip port Nothing, skipWords 3 s)]
        ("Multicast":mcast:port:localIp:_) ->
            [(UdpIn localIp port (Just mcast), skipWords 4 s)]
        _ -> []

instance ToJSON UdpIn where
    toJSON (UdpIn ip port Nothing) = toJSON $ "Unicast " ++ ip ++ " " ++ port
    toJSON (UdpIn localIp port (Just mcast)) = toJSON $
        "Multicast " ++ mcast ++ " " ++ port ++ " " ++ localIp

instance FromJSON UdpIn where
    parseJSON (Data.Aeson.String s) = case (words $ unpack s) of
        ("Unicast":ip:port:[]) -> return $ UdpIn ip port Nothing
        ("Multicast":mcast:port:localIp:[]) -> return $
            UdpIn localIp port (Just mcast)
        _ -> typeMismatch "UdpIn" (Data.Aeson.String s)
    parseJSON t = typeMismatch "UdpIn" t

data UdpOut = UdpOut Ip Port (Maybe (Mcast, Maybe TTL))
    deriving (Generic, Eq, Show)
instance ToJSON UdpOut
instance FromJSON UdpOut

udpInOptions :: Opt.Parser UdpIn
udpInOptions = UdpIn
    <$> Opt.strOption
        ( Opt.long "ip"
       <> Opt.metavar "IP"
       <> Opt.help "IP address"
        )
    <*> Opt.strOption
        ( Opt.long "port"
       <> Opt.metavar "PORT"
       <> Opt.help "Port number"
        )
    <*> Opt.optional (Opt.strOption
        ( Opt.long "multicast"
       <> Opt.metavar "IP"
       <> Opt.help "Join to multicast IP address"
        ))

udpOutOptions :: Opt.Parser UdpOut
udpOutOptions = UdpOut
    <$> Opt.strOption
        ( Opt.long "ip"
       <> Opt.metavar "IP"
       <> Opt.help "IP address (destination or local in case of multicast)"
        )
    <*> Opt.strOption
        ( Opt.long "port"
       <> Opt.metavar "PORT"
       <> Opt.help "Port number"
        )
    <*> Opt.optional mcast
  where
    mcast = (,)
        <$> Opt.strOption
            ( Opt.long "multicast"
           <> Opt.metavar "IP"
           <> Opt.help "Destination multicast IP address"
            )
        <*> Opt.optional (Opt.option Opt.auto
            ( Opt.long "ttl"
           <> Opt.metavar "TTL"
           <> Opt.help "IP TTL value"
            ))

-- | UDP network reader.
udpReader :: UdpIn -> Producer (BS.ByteString, Net.SockAddr) (SafeT IO) c
udpReader addr = bracket acquire Net.close action where

    acquire = do
        let (ip, port, mclocal) = case addr of
                UdpIn ip' port' Nothing -> (ip', port', Nothing)
                UdpIn ip' port' (Just mcast') -> (mcast', port', Just ip')
        (serveraddr:_) <- Net.getAddrInfo
            (Just (Net.defaultHints {Net.addrFlags = [Net.AI_PASSIVE]}))
            (Just ip)
            (Just port)
        sock <- Net.socket
            (Net.addrFamily serveraddr) Net.Datagram Net.defaultProtocol
        case mclocal of
            Nothing -> return ()
            Just _ -> do
                Net.setSocketOption sock Net.ReuseAddr 1
                Mcast.addMembership sock ip mclocal
        Net.bind sock (Net.addrAddress serveraddr)
        return sock

    action sock = forever $ do
        msg <- liftIO $ NB.recvFrom sock (2^(16::Int))
        yield msg

-- | UDP bytestring consumer (write to network).
udpWriter :: UdpOut -> Consumer BS.ByteString (SafeT IO) c
udpWriter (UdpOut ip port mMcast) = bracket acquire Net.close action where

    acquire = Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol

    action sock = do
        dst <- liftIO $ case mMcast of
            Nothing -> do
                (serveraddr:_) <- Net.getAddrInfo Nothing (Just ip) (Just port)
                return (Net.addrAddress serveraddr)
            Just (mcast, mTtl) -> do
                (serveraddr:_) <-
                    Net.getAddrInfo Nothing (Just mcast) (Just port)
                Mcast.setInterface sock ip
                case mTtl of
                    Nothing -> return ()
                    Just ttl -> Mcast.setTimeToLive sock ttl
                return (Net.addrAddress serveraddr)
        forever $ do
            s <- await
            liftIO $ NB.sendAllTo sock s dst

