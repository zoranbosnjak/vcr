{-# LANGUAGE OverloadedStrings #-}

-- | Simple UDP data generator - for test purposes.

module Main where

import           Control.Concurrent        (threadDelay)
import qualified Data.ByteString           as BS
import qualified Network.Multicast         as Mcast
import qualified Network.Socket            as Net
import qualified Network.Socket.ByteString as NB
import           Options.Applicative
import           UnliftIO

type Host = String
type Mcast = String
type Ip = String
type Port = String
type TTL = Int

type Payload = Int
type Rate = Double

data Destination
    = Unicast Ip Port
    | Multicast Ip Port Ip TTL
    deriving (Eq, Show)

parser :: Parser (Destination, Payload, Rate)
parser = (,,)
    <$> (parseUnicast <|> parseMulticast)
    <*> option auto (long "size" <> help "UDP payload bytes")
    <*> option auto (long "rate" <> help "packets per second")
  where
    parseUnicast = Unicast
        <$> strOption (long "unicast" <> help "Unicast Ip")
        <*> strOption (long "port" <> help "Port number")
    parseMulticast = Multicast
        <$> strOption (long "multicast" <> help "Multicast Ip")
        <*> strOption (long "port" <> help "Port number")
        <*> strOption (long "localif" <> help "Local interface Ip")
        <*> option auto (long "ttl" <> help "TTL value")

main :: IO ()
main = execParser options >>= \(out, payload, rate) -> do
    bracket (acquire out) release $ \(sock, dst) -> do
        let loop !n = do
                let msg = BS.singleton n <> BS.replicate (toEnum payload - 1) 0
                NB.sendAllTo sock msg dst
                threadDelay $ round $ (1000*1000) / rate
                loop $ succ n
        loop 0
  where
    options = info (parser <**> helper) ( progDesc "UDP generator")

    acquire addr = do
        let (ip, port, mLocal, mTTL) = case addr of
                Unicast ip' port' ->
                    (ip', port', Nothing, Nothing)
                Multicast mcast' port' local' ttl' ->
                    (mcast', port', Just local', Just ttl')

        (serveraddr:_) <- Net.getAddrInfo
            (Just (Net.defaultHints {Net.addrFlags = [Net.AI_PASSIVE]}))
            (Just ip)
            (Just port)

        sock <- Net.socket
            (Net.addrFamily serveraddr) Net.Datagram Net.defaultProtocol

        maybe (pure ()) (Mcast.setInterface sock) mLocal
        maybe (pure ()) (Mcast.setTimeToLive sock) mTTL
        pure (sock, Net.addrAddress serveraddr)

    release = Net.close . fst
