------------------
-- |
-- Module: Udp
--
-- This module provides common UDP definitions.
--

module Udp where

import           Control.Monad
import           Control.Exception (bracket)
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NB
import qualified Network.Multicast as Mcast
import qualified Options.Applicative as Opt
import           Streams

type Ip = String
type Port = String
type Mcast = String
type TTL = Int

-- | UDP (unicast or multicast)
data UdpIn = UdpIn Ip Port (Maybe Mcast) deriving (Eq, Show)
data UdpOut = UdpOut Ip Port (Maybe (Mcast, Maybe TTL)) deriving (Eq, Show)

newtype RxSocket = RxSocket Net.Socket
data TxSocket = TxSocket Net.Socket Net.SockAddr

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
rxSocket :: UdpIn -> IO RxSocket
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
    return $ RxSocket sock

-- | Receive data from the socket.
rxUdp :: RxSocket -> IO (BS.ByteString, Net.SockAddr)
rxUdp (RxSocket sock) = NB.recvFrom sock (2^(16::Int))

-- | UDP bytestring producer.
udpReader :: UdpIn -> Producer (BS.ByteString, Net.SockAddr)
udpReader addr = mkProducer action where
    acquire = rxSocket addr
    release = closeRxSock
    action produce = bracket acquire release $ \sock -> forever $ do
        rxUdp sock >>= produce

-- | Close Rx socket.
closeRxSock :: RxSocket -> IO ()
closeRxSock (RxSocket sock) = Net.close sock

-- | Open TX socket.
txSocket :: UdpOut -> IO TxSocket
txSocket (UdpOut ip port mMcast) = do
    sock <- Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol
    dst <- case mMcast of
        Nothing -> do
            (serveraddr:_) <- Net.getAddrInfo Nothing (Just ip) (Just port)
            return (Net.addrAddress serveraddr)
        Just (mcast, mTtl) -> do
            (serveraddr:_) <- Net.getAddrInfo Nothing (Just mcast) (Just port)
            Mcast.setInterface sock ip
            case mTtl of
                Nothing -> return ()
                Just ttl -> Mcast.setTimeToLive sock ttl
            return (Net.addrAddress serveraddr)
    return $ TxSocket sock dst

-- | Send data to the socket.
txUdp :: TxSocket -> BS.ByteString -> IO ()
txUdp (TxSocket sock dst) s = NB.sendAllTo sock s dst

{-
-- | UDP bytestring consumer.
udpWriter :: UdpOut -> Consumer BS.ByteString IO ()
udpWriter addr = do
    sock <- lift $ txSocket addr
    forever $ do
        s <- await
        lift $ txUdp sock s
-}

-- | Close Tx socket.
closeTxSock :: TxSocket -> IO ()
closeTxSock (TxSocket sock _dst) = Net.close sock

