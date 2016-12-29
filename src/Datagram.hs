-- datagram specifics

module Datagram where

data Datagram = Datagram
    { dData     :: BSL.ByteString
    , dSource   :: SockAddr
    }

