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

import Control.Exception (throwIO, Exception)
import Control.Monad (unless)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Builder.Internal as Opt.Int
import Options.Applicative.Types (OptReader(CmdReader))
import qualified System.Log.Logger as Log

-- local imports
import qualified Encodings as Enc

type Ip = String
type Port = String
type LocalIp = String
type TTL = Int
type URI = String
type ConnectTimeout = Double
type RetryTimeout = Double

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

-- | File storage.
data FileStore = FileStore FilePath Enc.EncodeFormat deriving (Eq, Show)

fileStoreOptions :: Opt.Parser FileStore
fileStoreOptions = FileStore
    <$> Opt.strOption
        ( Opt.long "path"
       <> Opt.metavar "FILE"
       <> Opt.help "Filename"
        )
    <*> Enc.encodeFormatOptions

-- | Server.
data Server = Server URI deriving (Eq, Show)

serverOptions :: Opt.Parser Server
serverOptions = Server
    <$> Opt.strOption
        (Opt.long "uri"
       <> Opt.metavar "URI"
       <> Opt.help "URI address"
        )

connectTimeoutOptions :: Opt.Parser ConnectTimeout
connectTimeoutOptions = Opt.option Opt.auto
    ( Opt.long "connect"
   <> Opt.metavar "SECONDS"
   <> Opt.help "Connect timeout"
   <> Opt.value 3
   <> Opt.showDefault
    )

retryTimeoutOptions :: Opt.Parser RetryTimeout
retryTimeoutOptions = Opt.option Opt.auto
    ( Opt.long "retry"
   <> Opt.metavar "SECONDS"
   <> Opt.help "Retry timeout"
   <> Opt.value 10
   <> Opt.showDefault
    )

-- | Log a message.
logM :: Log.Priority -> String -> IO ()
logM = Log.logM "vcr"

-- | Combine list of strings [s1,s2,...sn] to "s1 | s2 | ... | sn"
showOpt :: [String] -> String
showOpt [] = ""
showOpt lst = foldr1 (\a b -> a ++ " | " ++ b) (show <$> lst)

-- | This is a copy of 'subparser' function with the addition
-- of one more argument instead of fixed "COMMAND" string.
-- TODO: check if this can be implemented better or
-- move this function to Options.Applicative package.
subparserCmd :: String -> Opt.Int.Mod Opt.Int.CommandFields a -> Opt.Parser a
subparserCmd disp m = Opt.Int.mkParser d g rdr
  where
    Opt.Int.Mod _ d g = Opt.metavar disp `mappend` m
    (groupName, cmds, subs) = Opt.Int.mkCommand m
    rdr = CmdReader groupName cmds subs

newtype VcrError = VcrError String deriving (Eq, Typeable)
instance Exception VcrError
instance Show VcrError where
    show (VcrError err) = err

-- | Throw exception if the condition is not True
check :: Bool -> String -> IO ()
check condition err = unless condition $ throwIO (VcrError err)

