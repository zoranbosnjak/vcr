{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Control.Monad
--import           Data.Functor.Identity
--import           Data.Maybe
import qualified Data.ByteString as BS
import           System.IO.Temp
import           System.FilePath
--import           Data.Text (Text)
--
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

import           Pipes
import           Pipes.Safe
import qualified Pipes.Prelude as PP

import           Vcr
import           Streaming.Disk

import           TestCommon

{-
 - check TextFile operations with different line lengths
 - (in respect to Streaming.Disk.chunkSize - shorter and longer lines)
-}

-- rawFileProp (for any content... and other variants)
-- QC.testProperty "rawFile" $ \enc (lst :: [BS.ByteString]) -> QC.ioProperty $ do

rawFileSteps :: TestTree
rawFileSteps = testCaseSteps "rawFile" $ \step -> do
    forM_ [minBound..maxBound] $ \(enc :: FileEncoding) -> do
        withSystemTempDirectory "vcr-test" $ \base -> do
            samples' <- generate (arbitrary `suchThat` (\val -> length val > 0))

            let samples = fmap getTextLine samples'

                fa :: FileArchive BS.ByteString
                fa = FileArchive enc (base </> "recording")

                logger _ = return ()

                recorder :: Consumer BS.ByteString (SafeT IO) ()
                recorder = mkRecorder fa logger

                player :: Direction -> FileOffset -> Producer (FileOffset, BS.ByteString) (SafeT IO) FileOffset
                player = mkRawPlayer fa

            step "create recording"
            do
                runSafeT $ runEffect (each samples >-> recorder)

            step "check size"
            do
                result <- limits fa
                (a,b) <- case result of
                    Nothing -> assertFailure "empty file"
                    Just val -> return val
                assertEqual "i1" (0::FileOffset) a
                let rawLength = fromIntegral $ sum (BS.length <$> samples)
                    items = fromIntegral $ length samples
                assertEqual "i2" (rawLength + items) b

            step "replay forward/backward"
            do
                Just (a, b) <- limits fa

                (result1, ix1) <- runSafeT $ PP.toListM' $ player Forward a
                assertEqual "complete" (length samples) (length result1)
                assertEqual "final index1" b ix1

                (result2, ix2) <- runSafeT $ PP.toListM' $ player Backward b
                assertBool "reversed" (result1 == reverse result2)
                assertEqual "final index2" a ix2

                forM_ (zip samples result1) $ \(x, (i, y)) -> do
                    assertBool "index" (i >= a)
                    assertBool "index" (i <= b)
                    assertEqual "readback" x y

            step "replay from any valid index"
            -- TODO

propTests :: TestTree
propTests = testGroup "Property tests"
    [ -- rawFileProp
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ rawFileSteps
    ]

tests :: TestTree
tests = testGroup "Tests" [propTests, unitTests]

main :: IO ()
main = defaultMain tests

