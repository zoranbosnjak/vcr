------------------
-- |
-- Module: Udp
--
-- This module provides common UDP definitions.
--

module Udp
( Ip, Port, Mcast, TTL
, UdpIn, udpInOptions
, UdpOut, udpOutOptions
, rxSocket, rxUdp
) where

import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NB
import qualified Network.Multicast as Mcast
import qualified Options.Applicative as Opt

type Ip = String
type Port = String
type Mcast = String
type TTL = Int

-- | UDP (unicast or multicast)
data UdpIn = UdpIn Ip Port (Maybe Mcast) deriving (Eq, Show)
data UdpOut = UdpOut Ip Port (Maybe Mcast, (Maybe TTL)) deriving (Eq, Show)

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
udpOutOptions = undefined

-- | Open RX socket and join to multicast if required.
rxSocket :: UdpIn -> IO Net.Socket
rxSocket addr = do
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

-- | Receive data from the socket.
rxUdp :: Net.Socket -> IO (BS.ByteString, Net.SockAddr)
rxUdp sock = NB.recvFrom sock (2^(16::Int))

