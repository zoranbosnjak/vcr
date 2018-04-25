------------------
-- |
-- Module: Common
--
-- This module provides common VCR definitions.
--

module Common
( logM
, Log.Priority(..)
, VcrOptions(..)
, vcrOptions
, timeOptions
, subparserCmd
, throw
, check
, kiloMega
) where

import Control.Exception (throwIO, Exception)
import Control.Monad (unless)
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Builder.Internal as Opt.Int
import Options.Applicative.Types (OptReader(CmdReader))
import qualified System.Log.Logger as Log

------------------------------------------------------------------------------
-- Logging and error reporting.

newtype VcrError = VcrError String deriving (Eq, Typeable)
instance Exception VcrError
instance Show VcrError where
    show (VcrError err) = err

-- | Throw VCR exception.
throw :: String -> IO a
throw s = do
    let err = VcrError s
    logM Log.ERROR $ show err
    throwIO err

-- | Throw exception if the condition is not True.
check :: Bool -> String -> IO ()
check condition = unless condition . throw

-- | Log a message.
logM :: Log.Priority -> String -> IO ()
logM = Log.logM "vcr"

------------------------------------------------------------------------------
-- Support for commmand line option parsing.

-- | General VCR command line options.
data VcrOptions = VcrOptions
    { vcrOptVerbose :: Maybe Log.Priority
    , vcrOptSyslog  :: Maybe Log.Priority
    } deriving (Show)

-- | A parser for parsing general VCR command line options.
vcrOptions :: Opt.Parser VcrOptions
vcrOptions = VcrOptions
    <$> Opt.optional (Opt.option Opt.auto
        ( Opt.short 'v'
       <> Opt.long "verbose"
       <> Opt.metavar "LEVEL"
       <> Opt.help ("Set console verbosity level, one of: " ++ show levels)
        ))
    <*> Opt.optional (Opt.option Opt.auto
        ( Opt.long "syslog"
       <> Opt.metavar "LEVEL"
       <> Opt.help ("Set syslog verbosity level, one of: " ++ show levels)
        ))
  where
    levels = [minBound..maxBound] :: [Log.Priority]

-- | Options for start/stop time.
timeOptions :: String -> Opt.Parser UTCTime
timeOptions _s = undefined

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

-- | Helper function to convert eg. 1k -> 1024
kiloMega :: Opt.ReadM Integer
kiloMega = Opt.eitherReader $ \arg -> do
    let (a,b) = case last arg of
            'k' -> (init arg, 1)
            'M' -> (init arg, 2)
            'G' -> (init arg, 3)
            _ -> (arg, 0)
    case reads a of
        [(r, "")] -> return (r * (1024^(b::Int)))
        _         -> Left $ "cannot parse value `" ++ arg ++ "'"

