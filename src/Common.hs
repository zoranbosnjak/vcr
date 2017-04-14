------------------
-- |
-- Module: Common
--
-- This module provides common VCR definitions
--

module Common
-- (
-- TODO: add explicit exports of this module
{-)-} where

import Data.Monoid ((<>))
import qualified Options.Applicative as Opt
import qualified System.Log.Logger as Log
import Text.Read (readMaybe)

-- local imports
import qualified Encodings as Enc

type Ip = String
type Port = String
type LocalIp = String
type TTL = Int
type URI = String

-- | General VCR command line options.
data VcrOptions = VcrOptions
    { vcrOptVerbose :: Maybe Log.Priority
    } deriving (Show)

-- A parser for parsing general VCR command line options.
vcrOptions :: Opt.Parser VcrOptions
vcrOptions = VcrOptions
    <$> Opt.optional (Opt.option Opt.auto
        ( Opt.short 'v'
       <> Opt.long "verbose"
       <> Opt.metavar "LEVEL"
       <> Opt.help ("Set verbosity level, one of: " ++ show levels)
        ))
  where
    levels = [minBound..maxBound] :: [Log.Priority]

-- | UDP (unicast or multicast)
data Udp
    = Unicast Ip Port
    | Multicast Ip Port LocalIp (Maybe TTL)
    deriving (Eq)

instance Show Udp where
    show (Unicast ip port) = "unicast " ++ ip ++ " " ++ port
    show (Multicast ip port loc mttl) =
        "multicast " ++ ip ++ " " ++ port ++ " " ++ loc ++ case mttl of
            Nothing -> ""
            Just ttl -> " " ++ show ttl

instance Read Udp where
    readsPrec _ s = case words s of
        ["unicast", ip, port] -> [(Unicast ip port, "")]
        ["multicast", ip, port, loc] -> [(Multicast ip port loc Nothing, "")]
        ["multicast", _ip, _port, _loc, _ttl] -> undefined  -- TODO
        _ -> []

-- | Possible UDP options.
udpOptions :: [String]
udpOptions =
    [ "unicast <ip> <port>"
    , "multicast <ip> <port> <localIp>"
    , "multicast <ip> <port> <localIp> <ttl>"
    ]

-- | UDP command line parser helper.
udpAs :: Read a => Char -> String -> Opt.Parser a
udpAs short long = Opt.option (Opt.maybeReader readMaybe)
    ( Opt.short short
   <> Opt.long long
   <> Opt.metavar "ARG"
   <> Opt.help (showOpt udpOptions)
    )

-- | File storage.
data FileStore = FileStore FilePath Enc.EncodeFormat deriving (Eq)

instance Show FileStore where
    show = undefined

instance Read FileStore where
    readsPrec _ s = undefined

-- | Log a message.
logM :: Log.Priority -> String -> IO ()
logM = Log.logM "vcr"

-- | Combine list of strings [s1,s2,...sn] to "s1 | s2 | ... | sn"
showOpt :: [String] -> String
showOpt [] = ""
showOpt lst = foldr1 (\a b -> a ++ " | " ++ b) (show <$> lst)

