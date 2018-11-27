
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module TestPipes (testPipes) where

import Control.Monad
import Control.Exception
import Data.Foldable
import Control.Concurrent.STM
import Data.IORef
import qualified Data.Sequence as DS
import qualified Data.ByteString as BS
import System.IO
import System.Directory

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Safe

import Common as C
import Event
import Encodings
import File

testPipes :: TestTree
testPipes = testGroup "Pipes"
    [ testGroup "basic"
        [ testProperty "basic producer" propBasicProducer
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
rightToBuffer :: (MonadIO m) => IORef (DS.Seq a) -> Consumer (Either t a) m ()
rightToBuffer buf = forever $ await >>= \case
    Left _ -> return ()
    Right x -> liftIO $ modifyIORef buf (DS.|> x)

propBasicProducer :: Int -> Property
propBasicProducer n = ioProperty $ do
    buf <- newTVarIO DS.empty
    runEffect $ for producer $ \x -> do
        liftIO $ atomically $ modifyTVar buf (DS.|> x)
    result <- toList <$> readTVarIO buf
    return $ result === orig
  where
    orig = take n [1..] :: [Int]
    producer = each orig

-- | Producer produces nothing
propBasicPipe0 :: Property
propBasicPipe0 = ioProperty $ do
    runEffect $ producer >-> C.drain
    return True
  where
    producer :: (Monad m) => Producer () m ()
    producer = return ()

-- | Producer finish early (1 element)
propBasicPipe1a :: Property
propBasicPipe1a = ioProperty $ do
    runEffect $ producer >-> C.drain
    return True
  where
    producer = do
        yield ()

-- | Consumer finish early (1 element)
propBasicPipe1b :: Property
propBasicPipe1b = ioProperty $ do
    runEffect $ producer >-> consumer
    return True
  where
    consumer = do
        _ <- await
        return ()
    producer = forever $ do
        yield ()

propBasicPipe2 :: Int -> Property
propBasicPipe2 n = ioProperty $ do
    buf <- newTVarIO DS.empty
    let producer = each orig
        consumer = forever $ do
            x <- await
            liftIO $ atomically $ modifyTVar buf (DS.|> x)
    runEffect $ producer >-> PP.map id >-> consumer
    result <- toList <$> atomically (readTVar buf)
    return $ result === orig
  where
    orig = take n [1..] :: [Int]

propFileEncodeDecode :: Int -> EncodeFormat -> [Event] -> Property
propFileEncodeDecode chunkSize fmt orig = ioProperty $ withTempFile $ \fn -> do
    -- write data to a file
    runSafeT $ runEffect $
        each orig
        >-> toByteString fmt
        >-> rotatingFileWriter (streamPath fn) Nothing (\_ -> return ())

    -- read data
    buf <- newIORef DS.empty
    runSafeT $ runEffect $
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

