
{-# LANGUAGE LambdaCase #-}

module TestStreams (testStreams) where

import Control.Monad
import Control.Exception
import Data.Foldable
import Control.Concurrent.STM
import Data.IORef
import qualified Data.Sequence as DS
import qualified Data.ByteString as BS
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import System.IO
import System.Directory

import Event
import Encodings
import Streams
import File

testStreams :: Test
testStreams = testGroup "Streams"
    [ testGroup "basic"
        [ testProperty "basic producer" propBasicProducer
        , testProperty "basic consumer" propBasicConsumer
        , testProperty "basic pipe 0" propBasicPipe0
        , testProperty "basic pipe 1a" propBasicPipe1a
        , testProperty "basic pipe 1b" propBasicPipe1b
        , testProperty "basic pipe 1b" propBasicPipe2
        ]
    , testGroup "encode/decode"
        [ testProperty "file" propFileEncodeDecode
        ]
    ]

-- | Consume data and store Right items to buffer (helper function for tests).
rightToBuffer :: IORef (DS.Seq a) -> Consumer (Either t a) ()
rightToBuffer buf = consumeToIO $ \case
    Left _ -> return ()
    Right x -> modifyIORef buf (DS.|> x)

propBasicProducer :: Int -> Property
propBasicProducer n = ioProperty $ do
    buf <- newTVarIO DS.empty
    let produce = \x -> modifyTVar buf (DS.|> x)
    (streamAction producer) noConsume produce
    result <- toList <$> readTVarIO buf
    return $ result === orig
  where
    orig = take n [1..] :: [Int]
    producer = fromFoldable orig

propBasicConsumer :: Int -> Property
propBasicConsumer n = ioProperty $ do
    buf <- newTVarIO $ DS.fromList orig
    let consume = do
            s <- readTVar buf
            case DS.null s of
                True -> return $ EndOfData []
                False -> do
                    writeTVar buf (DS.drop 1 s)
                    return $ Message (DS.index s 0)
    result <- (streamAction consumer) consume noProduce
    return $ result === orig
  where
    orig = take n [1..] :: [Int]

    consumer :: Consumer a [a]
    consumer = mkConsumer (loop []) where
        loop acc consume = do
            atomically consume >>= \case
                EndOfData _ -> return $ reverse acc
                Message msg -> loop (msg:acc) consume

-- | Producer produces nothing
propBasicPipe0 :: Property
propBasicPipe0 = ioProperty $ do
    runStream $ producer >-> drain
    return True
  where
    producer = mkProducer $ \_produce -> return ()

-- | Producer finish early (1 element)
propBasicPipe1a :: Property
propBasicPipe1a = ioProperty $ do
    runStream $ producer >-> drain
    return True
  where
    producer = mkProducer $ \produce -> do
        atomically $ produce ()

-- | Consumer finish early (1 element)
propBasicPipe1b :: Property
propBasicPipe1b = ioProperty $ do
    runStream $ producer >-> consumer
    return True
  where
    consumer = mkConsumer $ \consume -> do
        _msg <- atomically consume
        return ()
    producer = mkProducer $ \produce -> forever $ do
        atomically $ produce ()

propBasicPipe2 :: Int -> Property
propBasicPipe2 n = ioProperty $ do
    buf <- newTVarIO DS.empty
    let producer = fromFoldable orig
        consumer = consumeToIO $ \x -> atomically $ modifyTVar buf (DS.|> x)
    runStream_ $ producer >-> Streams.map id >-> consumer
    result <- toList <$> atomically (readTVar buf)
    return $ result === orig
  where
    orig = take n [1..] :: [Int]

propFileEncodeDecode :: Int -> EncodeFormat -> [Event] -> Property
propFileEncodeDecode chunkSize fmt orig = ioProperty $ withTempFile $ \fn -> do
    -- write data to a file
    runStream_ $
        fromFoldable orig
        >-> toByteString fmt
        >-> fileWriter (FileStore fn) Nothing (\_ -> return ())

    -- read data
    buf <- newIORef DS.empty
    runStream_ $
        fileReaderChunks (max chunkSize 1) (FileStore fn)
        >-> Encodings.fromByteString maxEventSize fmt
        >-> rightToBuffer buf

    -- fetch data from buffer and compare with the original
    result <- toList <$> readIORef buf
    return $ result === orig
  where
    maxEventSize = maximum [BS.length (encode fmt x) | x <- orig]
    withTempFile act = Control.Exception.bracket getName removeFile act where
        getName = do
            randDir <- getTemporaryDirectory >>= canonicalizePath
            (fn,fh) <- openTempFile randDir "recording.tmp"
            hClose fh
            return fn

