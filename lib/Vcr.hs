
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Vcr where

import           GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Network.Socket as Net
import           Data.Aeson

-- local imports
import           Event
import           Common

data UdpContent = UdpContent
    { udpDatagram  :: BS.ByteString
    , udpSender    :: (Net.HostName, Net.ServiceName)
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

-- This is what get's recorded when listening on UDP
type UdpEvent = Event UdpContent

