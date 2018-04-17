
module TestStreams (testStreams) where

import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Data.Foldable
import Data.IORef
import qualified Data.Sequence as DS
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
        [ testProperty "basic" propBasic
        ]
    , testGroup "encode/decode"
        [ testProperty "file" propFileEncodeDecode
        ]
    ]

-- | Consume data and store to buffer (helper function for tests).
toBuffer :: IORef (DS.Seq a) -> Consumer a
toBuffer buf = mkConsumer $ \consume -> forever $ do
    val <- consume Clear
    liftIO $ modifyIORef buf (DS.|> val)

-- | Consume data and store Right items to buffer (helper function for tests).
rightToBuffer :: IORef (DS.Seq a) -> Consumer (Either t a)
rightToBuffer buf = mkConsumer $ \consume -> forever $ do
    rv <- consume Clear
    case rv of
        Left _e -> return ()
        Right a -> liftIO $ modifyIORef buf (DS.|> a)

propBasic :: Int -> Property
propBasic n = ioProperty $ do
    buffer <- newIORef DS.empty
    runStream $ src >-> pipe >-> toBuffer buffer
    result <- toList <$> readIORef buffer
    return $ result === orig
  where
    orig = take n [1..] :: [Int]
    src = fromFoldable orig
    pipe = mkPipe $ \consume produce -> forever $ do
        consume Clear >>= produce

propFileEncodeDecode :: EncodeFormat -> [Event] -> Property
propFileEncodeDecode fmt orig = ioProperty $ withTempFile $ \fn -> do
    -- write data to a file
    runStream $
        fromFoldable orig
        >-> toByteString fmt
        >-> fileWriter (FileStore fn) Nothing
        >-> drain

    -- read data
    buffer <- newIORef DS.empty
    runStream $
        fileReaderChunks 32752 (FileStore fn)
        >-> Encodings.fromByteString maxBound fmt
        >-> rightToBuffer buffer

    -- fetch data from buffer and compare with the original
    result <- toList <$> readIORef buffer
    return $ result === orig
  where
    withTempFile act = Control.Exception.bracket getName removeFile act where
        getName = do
            randDir <- getTemporaryDirectory >>= canonicalizePath
            (fn,fh) <- openTempFile randDir "recording.tmp"
            hClose fh
            return fn

