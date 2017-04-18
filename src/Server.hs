------------------
-- |
-- Module: Server
--
-- This module provides server access definitions.
--

module Server
( ServerConnection(..)
, serverConnectionOptions
) where

-- standard imports
import Data.Monoid ((<>))
import qualified Options.Applicative as Opt

-- local imports

type URI = String
type ConnectTimeout = Double
type RetryTimeout = Double

-- | Server connection pool.
data ServerConnection = ServerConnection
    { serverPool      :: [Server]
    , connectTimeout  :: ConnectTimeout
    , retryTimeout    :: RetryTimeout
    } deriving (Eq, Show)

-- | Single Server.
data Server = Server URI deriving (Eq, Show)

serverConnectionOptions :: Opt.Parser ServerConnection
serverConnectionOptions = ServerConnection
    <$> Opt.some serverOptions
    <*> connectTimeoutOptions
    <*> retryTimeoutOptions

serverOptions :: Opt.Parser Server
serverOptions = Server
    <$> Opt.strOption
        (Opt.long "uri"
       <> Opt.metavar "URI"
       <> Opt.help "Server URI address"
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

