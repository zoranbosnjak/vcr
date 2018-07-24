----------------
-- |
-- Module:      Buffer
--
-- Buffer manipulations
--

{-# LANGUAGE DeriveGeneric #-}

module Buffer where

import           GHC.Generics (Generic)
import qualified System.Clock
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Sequence as DS
import           Data.Foldable (toList)
import           Control.Concurrent.STM as STM
import           Data.Monoid ((<>))
import qualified Options.Applicative as Opt
import           Data.Aeson (ToJSON, FromJSON)

-- import qualified Event
import qualified Common as C
import qualified Encodings as Enc
import           Streams

-- | Threshold, limit by different properties.
data Threshold = Threshold
    { thLength      :: Maybe Int
    , thBytes       :: Maybe Integer
    , thSeconds     :: Maybe Double
    } deriving (Generic, Eq, Show)

instance ToJSON Threshold
instance FromJSON Threshold

instance Monoid Threshold where
    mempty = Threshold Nothing Nothing Nothing
    Threshold a1 b1 c1 `mappend` Threshold a2 b2 c2 = Threshold
        (lower a1 a2)
        (lower b1 b2)
        (lower c1 c2)
      where
        lower Nothing x = x
        lower x Nothing = x
        lower (Just a) (Just b) = Just (min a b)

thresholdValid :: Threshold -> Bool
thresholdValid th = not (th == mempty)

thresholdOptions :: String -> Opt.Parser Threshold
thresholdOptions s = threshold
    <$> Opt.optional (Opt.option Opt.auto
        ( Opt.long (s++"Events")
       <> Opt.help "number of events")
        )
    <*> Opt.optional (Opt.option C.kiloMega
        ( Opt.long (s++"Bytes")
       <> Opt.help "size in bytes")
        )
    <*> Opt.optional (Opt.option Opt.auto
        ( Opt.long (s++"Seconds")
       <> Opt.help "seconds")
        )

-- | Make sure that th1 (readTh) will react before th2 (appendTh), equal is OK.
isBelow :: Threshold -> Threshold -> Bool
isBelow (Threshold a1 b1 c1) (Threshold a2 b2 c2) = and [a1<!a2, b1<!b2, c1<!c2]
  where
    Just a <! Just b = a <= b
    _ <! Nothing = True
    Nothing <! _ = False

threshold :: (Maybe Int) -> (Maybe Integer) -> (Maybe Double) -> Threshold
threshold = Threshold

-- | Concatinate incomming items.
concatinate :: (Monoid a) => Pipe [a] a
concatinate = Streams.map mconcat

-- | Buffer will hold data until the threshold th1 is reached.
-- Then it will send all buffered data as a list.
-- It would drop all data if th2 is reached.
holdBuffer :: (Enc.HasSize a) => Threshold -> Threshold -> ([a] -> IO ())
    -> Pipe a [a]
holdBuffer th1 th2 dropAct = case thresholdValid th1 of
    False -> Streams.map (\val -> [val])
    True -> mkBridge acquire release rx tx flush
  where
    now = System.Clock.toNanoSecs <$> System.Clock.getTime System.Clock.Boottime

    acquire = (,,)
        <$> newTVarIO DS.empty      -- buffer
        <*> (now >>= newTVarIO)     -- timestamp of last transmit
        <*> newTVarIO (0::Integer)  -- size counter

    release _ = return ()

    rx (buf, tOut, size) = mkConsumer $ \consume -> forever $ do
        msg <- consume Clear
        t0 <- liftIO now

        dropped <- liftIO $ atomically $ do
            content <- (DS.|> msg) <$> readTVar buf
            t <- readTVar tOut
            bytes <- (+ (Enc.sizeOf msg)) <$> readTVar size

            let timeout = case thSeconds th2 of
                    Nothing -> False
                    Just dt ->
                        let dtNs = round $ dt * (10^(9::Int))
                        in (t0 - t) > dtNs

                lengthOverflow = case thLength th2 of
                    Nothing -> False
                    Just top -> DS.length content > top

                sizeOverflow = case thBytes th2 of
                    Nothing -> False
                    Just top -> bytes > top

            case lengthOverflow || sizeOverflow || timeout of
                False -> do
                    writeTVar buf content
                    writeTVar size bytes
                    return []
                True -> do
                    writeTVar buf DS.empty
                    writeTVar size 0
                    writeTVar tOut t0
                    return $ toList content
        liftIO $ dropAct dropped

    tx (buf, tOut, size) = mkProducer $ \produce -> forever $ do
        timeout <- liftIO $ case thSeconds th1 of
            Nothing -> newTVarIO False
            Just val -> registerDelay $ (round val) * 1000000
        items <- liftIO $ atomically $ do
            c1 <- case thLength th1 of
                Nothing -> return False
                Just val -> (>= val) . DS.length <$> readTVar buf
            c2 <- case thBytes th1 of
                Nothing -> return False
                Just val -> (>= val) <$> readTVar size
            c3 <- readTVar timeout
            case (c1 || c2 || c3) of
                False -> retry
                True -> do
                    content <- readTVar buf
                    case DS.null content of
                        True -> retry
                        False -> do
                            writeTVar buf DS.empty
                            writeTVar size 0
                            return $ toList content
        produce items
        liftIO (now >>= (atomically . writeTVar tOut))

    flush (buf, _tOut, _size) = mkProducer $ \produce -> do
        items <- do
            content <- liftIO $ atomically $ readTVar buf
            return $ toList content
        produce items

