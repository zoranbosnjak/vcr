----------------
-- |
-- Module:      Buffer
--
-- Buffer manipulations
--

module Buffer where

import           Control.Concurrent.Async (async, cancel)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (when)
import qualified Data.Sequence as DS
import           Data.Foldable (toList)
import           Data.Maybe (isJust)
import           Control.Concurrent.STM as STM
import           Control.Monad.Trans.Except
import           Control.Exception (mask)
import           Data.Monoid ((<>))
import qualified Options.Applicative as Opt

-- import qualified Event
import qualified Common as C
import qualified Encodings as Enc
import           Streams

-- | Threshold, limit by different properties.
data Threshold = Threshold
    { thLength      :: Maybe Int
    , thBytes       :: Maybe Integer
    , thSeconds     :: Maybe Double
    } deriving (Eq, Show)

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

activeThrashold :: Threshold -> Bool
activeThrashold (Threshold a b c) = or [isJust a, isJust b, isJust c]

-- | Buffer will hold data until the threshold is reached.
-- Then it will send all buffered data as a list.
holdBuffer :: (Enc.HasSize a) => Threshold -> Pipe a [a]
holdBuffer th = case thresholdValid th of
    False -> Streams.map (\val -> [val])
    True -> mkPipe action
  where
    action consume produce = do
        buffer  <- liftIO $ newTVarIO DS.empty
        cnt     <- liftIO $ newTVarIO (0::Int)
        bytes   <- liftIO $ newTVarIO (0::Integer)

        -- write from intermediate buffer via produce function
        let writerLoop = forever $ do
                timeout <- case thSeconds th of
                    Nothing -> newTVarIO False
                    Just val -> registerDelay $ (round val) * 1000000
                items <- atomically $ do
                    c1 <- case thLength th of
                        Nothing -> return False
                        Just val -> (>= val) <$> readTVar cnt
                    c2 <- case thBytes th of
                        Nothing -> return False
                        Just val -> (>= val) <$> readTVar bytes
                    c3 <- readTVar timeout
                    case (c1 || c2 || c3) of
                        False -> retry
                        True -> do
                            content <- readTVar buffer
                            case DS.null content of
                                True -> retry
                                False -> return $ toList content

                _ <- mask $ \_restore -> do
                    atomically $ do
                        writeTVar buffer DS.empty
                        writeTVar cnt 0
                        writeTVar bytes 0
                    runExceptT $ produce items
                return ()

        let readerLoop writer = do
                mMsg <- catchE (Just <$> consume Clear) (const $ return Nothing)
                case mMsg of
                    Nothing -> do
                        liftIO $ cancel writer
                        content <- liftIO $ atomically $ readTVar buffer
                        when (not $ DS.null content) $ do
                            produce $ toList content
                    Just msg -> do
                        liftIO $ atomically $ do
                            modifyTVar buffer (\s -> s DS.|> msg)
                            modifyTVar cnt succ
                            modifyTVar bytes (+ (Enc.sizeOf msg))
                        readerLoop writer

        bracket (async writerLoop) cancel readerLoop

-- | Concatinate incomming items.
concatinate :: (Monoid a) => Pipe [a] a
concatinate = Streams.map mconcat

