
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Udp where

import           GHC.Generics (Generic)
import           Control.Monad
import           Data.Text as Text
import qualified Data.ByteString as BS
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NB
import qualified Network.Multicast as Mcast
import           Data.Aeson
import           Pipes
import           Pipes.Safe

type Host = Text
type Mcast = Text
type Ip = Text
type Port = Text
type TTL = Int

data UdpIn
    = UdpInUnicast Ip Port
    | UdpInMulticast Mcast Port (Maybe Ip)
    deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data UdpOut
    = UdpOutUnicast Ip Port
    | UdpOutMulticast Ip Port (Maybe Ip) (Maybe TTL)
    deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | UDP network reader.
udpReader :: UdpIn -> Producer (BS.ByteString, Net.SockAddr) (SafeT IO) c
udpReader addr = bracket acquire Net.close action where

    acquire = do
        let (ip, port, mc) = case addr of
                UdpInUnicast ip' port' ->
                    (Text.unpack ip', Text.unpack port', Nothing)
                UdpInMulticast mcast' port' m' ->
                    (Text.unpack mcast', Text.unpack port',
                        Just (Text.unpack <$> m'))
        (serveraddr:_) <- Net.getAddrInfo
            (Just (Net.defaultHints {Net.addrFlags = [Net.AI_PASSIVE]}))
            (Just ip)
            (Just port)
        sock <- Net.socket
            (Net.addrFamily serveraddr) Net.Datagram Net.defaultProtocol
        case mc of
            Nothing -> return ()
            Just mloc -> do
                Net.setSocketOption sock Net.ReuseAddr 1
                Mcast.addMembership sock ip mloc
        Net.bind sock (Net.addrAddress serveraddr)
        return sock

    action sock = forever $ do
        msg <- liftIO $ NB.recvFrom sock (2^(16::Int))
        yield msg

