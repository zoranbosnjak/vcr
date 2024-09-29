{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements UDP input (Producer) and output (Consumer).
-- UDP unicast and multicast modes are supported.

module Udp where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString           as BS
import           Data.Text                 as Text
import           GHC.Generics              (Generic)
import qualified Network.Multicast         as Mcast
import qualified Network.Socket            as Net
import qualified Network.Socket.ByteString as NB
import           Pipes
import           Pipes.Safe
import qualified UnliftIO                  as UIO

-- local imports
import           Common

type Host = Text
type Mcast = Text
type Ip = Text
type Port = Text
type TTL = Int

data UdpContent = UdpContent
    { udpDatagram :: BS.ByteString
    , udpSender   :: (Net.HostName, Net.ServiceName)
    } deriving (Generic, Eq, Show)

instance ToJSON UdpContent where
    toJSON (UdpContent datagram sender) = object
        [ "data"    .= hexlify datagram
        , "sender"  .= sender
        ]

instance FromJSON UdpContent where
    parseJSON = withObject "UdpContent" $ \v -> UdpContent
        <$> parseData (v .: "data")
        <*> v .: "sender"
      where
        parseData px = do
            s <- px
            maybe (fail "unable to parse") pure (unhexlify s)

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
udpReader addr = bracket acquire Net.close action
  where
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
            Nothing -> pure ()
            Just mloc -> do
                Net.setSocketOption sock Net.ReuseAddr 1
                Mcast.addMembership sock ip mloc
        UIO.onException
            (Net.bind sock (Net.addrAddress serveraddr))
            (Net.close sock)
        pure sock

    action sock = forever $ do
        msg <- liftIO $ NB.recvFrom sock (2^(16::Int))
        yield msg

-- | UDP network writer.
udpWriter :: UdpOut -> Consumer BS.ByteString (SafeT IO) c
udpWriter addr = bracket acquire (Net.close . fst) action
  where
    acquire = do
        let (ip, port, mLocal, mTTL) = case addr of
                UdpOutUnicast ip' port' ->
                    ( Text.unpack ip', Text.unpack port', Nothing, Nothing)
                UdpOutMulticast mcast' port' mLocal' mTTL' ->
                    ( Text.unpack mcast', Text.unpack port'
                    , Text.unpack <$> mLocal' , mTTL')
        (serveraddr:_) <- Net.getAddrInfo
            (Just (Net.defaultHints {Net.addrFlags = [Net.AI_PASSIVE]}))
            (Just ip)
            (Just port)
        sock <- Net.socket
            (Net.addrFamily serveraddr) Net.Datagram Net.defaultProtocol
        maybe (pure ()) (Mcast.setInterface sock) mLocal
        maybe (pure ()) (Mcast.setTimeToLive sock) mTTL
        pure (sock, Net.addrAddress serveraddr)

    action (sock, dst) = forever $ do
        msg <- await
        liftIO $ NB.sendAllTo sock msg dst

