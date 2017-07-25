------------------
-- |
-- Module: Server
--
-- This module provides server access definitions.
--

module Server
{-
( ServerConnection(..)
, serverConnectionOptions
) -} where

-- standard imports
import Data.Monoid ((<>))
import qualified Options.Applicative as Opt

-- import Network.HTTP.Simple

-- local imports

newtype URI = URI String deriving (Eq, Show)
type ConnectTimeout = Double
type RetryTimeout = Double

-- | Server connection pool.
data ServerConnection = ServerConnection
    { serverPool      :: [URI]
    , connectTimeout  :: ConnectTimeout
    , retryTimeout    :: RetryTimeout
    } deriving (Eq, Show)

serverConnectionOptions :: Opt.Parser ServerConnection
serverConnectionOptions = ServerConnection
    <$> Opt.some serverOptions
    <*> connectTimeoutOptions
    <*> retryTimeoutOptions

serverOptions :: Opt.Parser URI
serverOptions = URI
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

{-
requestMethod????
sendHttp ip port evts = do
    -- TODO: add proper content type
    request <- parseRequest $ "PUT http://"++ip++":"++port++"/events"
    let request' = setRequestBodyJSON evts $ request
        retryWith s = do
            tellIO NOTICE s
            threadDelaySec 3
            process
        process = do
            eResponse <- try (httpLBS request')
            case eResponse of
                Left (SomeException _e) ->
                    retryWith "Unable to connect."
                Right resp -> do
                    case getResponseStatusCode resp of
                        200 -> tellIO DEBUG "Request processed."
                        _ -> do retryWith $ show resp
    process
-}

