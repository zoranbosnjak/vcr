------------------
-- |
-- Module: Udp
--
-- This module provides common UDP definitions.
--

module Udp
-- (
-- TODO: add explicit exports of this module
{-)-} where

import Data.Monoid ((<>))
import qualified Options.Applicative as Opt

type Ip = String
type Port = String
type LocalIp = String
type TTL = Int

-- | UDP (unicast or multicast)
data UdpIn = UdpIn Ip Port (Maybe Ip) deriving (Eq, Show)
data UdpOut = UdpOut Ip Port (Maybe Ip, (Maybe TTL)) deriving (Eq, Show)

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

