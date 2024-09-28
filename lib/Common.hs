{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Common functions and definitions.

module Common
    ( Prog, Args, Version, Command, GhcBase, WxcLib
    , threadDelaySec
    , runAll
    , periodic
    , runMaybe
    , doNothing
    , whenSpecified
    , restartOnChange
    , hexlify, unhexlify
    , newline
    )
  where

import           Control.Monad
import           Control.Concurrent.STM
import           Control.Concurrent (threadDelay)
import           Data.Bool
import           UnliftIO.Async (race)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import           Data.Word (Word8)

type Prog = String
type Args = [String]
type Version = String
type GhcBase = String
type WxcLib = String
type Command = Prog -> Args -> Version -> GhcBase -> WxcLib -> IO ()

-- | Wait for given number of seconds.
threadDelaySec :: Double -> IO ()
threadDelaySec = threadDelay . round . (1000000*)

-- | Run action periodically (with delay in between).
periodic :: Double -> IO a -> IO b
periodic period act = forever (act >> threadDelaySec period)

-- | Sleep indefinetly.
doNothing :: IO a
doNothing = periodic 1.0 $ return ()

-- | Run all processes concurrently, return index of a terminated process.
runAll :: [IO ()] -> IO Int
runAll lst = go $ zip [0..] lst where
    go [] = doNothing >> return (-1)
    go ((ix,p):[]) = p >> return ix
    go ((ix,p):rest) = race p (go rest) >>= \case
        Left _a -> return ix
        Right b -> return b

-- | Run action if given argument is (Just x).
runMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
runMaybe mVal act = maybe (return ()) act mVal

-- | Run process if given argument is (Just x), otherwise run empty loop.
whenSpecified :: (t -> IO a) -> Maybe t -> IO a
whenSpecified _act Nothing = doNothing
whenSpecified act (Just val) = act val

-- | Restart process on STM value change.
restartOnChange :: Eq c => STM a -> (a -> c) -> (a -> IO b) -> IO b
restartOnChange getVar compareValue act = atomically getVar >>= go where
    go x = do
        result <- race (act x) $ atomically $ do
            y <- getVar
            bool (return y) retry (compareValue y == compareValue x)
        case result of
            Left a -> return a
            Right y -> go y

-- | Convert bytestring to hex representation.
hexlify :: BS.ByteString -> String
hexlify = BS8.unpack . B16.encode

-- | Convert hex representation back to a bytestring.
unhexlify :: String -> Maybe BS.ByteString
unhexlify = either (const Nothing) Just . B16.decode . BS8.pack

-- | Newline character, represented as Word8 (ByteString)
newline :: Word8
newline = BS.head $ BS8.singleton '\n'
