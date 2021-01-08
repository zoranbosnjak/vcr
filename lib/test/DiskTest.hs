{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module DiskTest where

import           Control.Monad
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString as BS
import           System.IO.Temp
import           System.FilePath
import           System.Random
import           System.IO
import           Data.Time
import           Data.Aeson

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

import           Pipes as P
import           Pipes.Safe
import qualified Pipes.Prelude as PP

import           Vcr
import           Time
import           Streaming.Disk

import           TestCommon

fileSuffixP :: TestTree
fileSuffixP = QC.testProperty "file suffix" $ \(t::UtcTime) (dir::FilePath) (rec::FilePath) ->
    let base = dir </> rec
        fileSuffix = utcToFileSuffix t
        fileName = fileSuffixToFileName base fileSuffix
        fileSuffix' = getFileSuffix base fileName
    in (fileSuffix' == Just fileSuffix)

lineSize :: Int
lineSize = 2 * Streaming.Disk.chunkSize

lineGenerator :: FileEncoding -> Gen BS.ByteString
lineGenerator enc = resize lineSize $ genLine $ (/=) (BS.c2w $ delimiter enc)

rawVcr :: (Recorder s (SafeT IO) BS.ByteString,
    Player s (SafeT IO) BS.ByteString, Indexed s IO) =>
    TestName -> (FilePath -> FileEncoding -> s) -> TestTree
rawVcr title mkArchive = testCaseSteps title $ \step -> do
    forM_ [minBound..maxBound] $ \(enc :: FileEncoding) -> do
        samples :: [BS.ByteString] <- generate (listOf1 $ lineGenerator enc)
        step $ "gen"
            <> ", lines: " <> show (length samples)
            <> ", total size: " <> show (sum (fmap BS.length samples))

        withSystemTempDirectory "vcr-test" $ \base -> do
            let archive  = mkArchive base enc
                logger _ = return ()
                recorder = mkRecorder archive logger
                player   = mkPlayer archive

            step "create recording"
            do
                runSafeT $ runEffect (each samples >-> recorder)

            step "replay forward/backward"
            readback <- do
                (a, b) <- liftIO $ limits archive

                result1 <- runSafeT $ PP.toListM $ player Forward a
                assertEqual "complete" (length samples) (length result1)

                result2 <- runSafeT $ PP.toListM $ player Backward b
                assertBool "reversed" (result1 == reverse result2)

                forM_ (zip samples result1) $ \(x, (i, y)) -> do
                    assertBool "index" (i >= a)
                    assertBool "index" (i <= b)
                    assertEqual "readback" x y

                return result1

            step "replay from any valid index"
            do
                let validIndices = fmap fst readback

                step "forward"
                forM_ (zip [0..] validIndices) $ \(n,ix) -> do
                    let expected = drop n readback
                    result <- runSafeT $ PP.toListM $ player Forward ix
                    assertBool "same result" (expected == result)

                step "backward"
                forM_ (zip [0..] validIndices) $ \(n,ix) -> do
                    let expected = reverse $ take n readback
                    result <- runSafeT $ PP.toListM $ player Backward ix
                    assertBool "same result" (expected == result)

dirRotate :: Int -> TestTree
dirRotate numFiles = testCaseSteps title $ \step -> do
    forM_ [minBound..maxBound] $ \(enc :: FileEncoding) -> do
        withSystemTempDirectory "vcr-test" $ \base' -> do
            let base = base' </> "rec"
                archive = DirectoryArchive enc base
                player = mkPlayer archive

            step "create random recording files"
            t0 <- generate arbitrary
            let tRotate = take numFiles $ iterate (addUTCTime 3600) t0
            samples <- forM tRotate $ \t -> do
                samples :: [BS.ByteString] <- generate (listOf1 $ lineGenerator enc)
                let fs = utcToFileSuffix t
                    f  = fileSuffixToFileName base fs
                withFile f WriteMode $ \h -> do
                    mapM_ (writeBytes enc h) samples
                return samples

            step "readback"
            readback <- do
                (a, _b) <- limits archive
                runSafeT $ PP.toListM $ player Forward a
            assertEqual "readback" (join samples) (fmap snd readback)

            step "replay from any valid index"
            do
                let validIndices = fmap fst readback

                step "forward"
                forM_ (zip [0..] validIndices) $ \(n,ix) -> do
                    let expected = drop n readback
                    result <- runSafeT $ PP.toListM $ player Forward ix
                    assertBool "same result" (expected == result)

                step "backward"
                forM_ (zip [0..] validIndices) $ \(n,ix) -> do
                    let expected = reverse $ take n readback
                    result <- runSafeT $ PP.toListM $ player Backward ix
                    assertBool "same result" (expected == result)
  where
    title = "read from " <> show numFiles <> " rotated file(s)"

-- | Save some items to a file, using a generator
-- Return the samples and new generator.
saveFile :: ToJSON a =>
    Int
    -> FileEncoding
    -> (Producer a IO r, [a])
    -> FilePath
    -> IO (Producer a IO r, [a])
saveFile nMax enc (gen0, acc0) f = do
    n <- randomRIO (1, nMax)
    withFile f WriteMode $ go n (gen0, acc0)
      where
        go 0 (gen, acc) _h = return (gen, acc)
        go n (gen, acc) h = do
            Right (event, gen') <- P.next gen
            writeBytes enc h $ encodeJSON event
            go (pred n) (gen', event:acc) h

mkEventGenerator :: UtcTime -> IO (Producer (Event ()) IO b)
mkEventGenerator t = do
    e0 <- do
        event :: Event () <- generate arbitrary
        return $ event { eTimeUtc = t }

    let nextEvent e = do
            dt <- randomRIO (1000, 1000*1000)
            return $ e
                { eTimeMono = eTimeMono e + dt
                , eTimeUtc = addMonoTimeNS dt (eTimeUtc e)
                }
    return $ eventGen nextEvent e0

-- | Helper function to find events in any archive.
locateEvents ::
    (Player s (SafeT IO) (Event ()), HasItem s IO (Event ()), Show (Index s)) =>
    (String -> IO ()) -> UtcTime -> s -> [Event ()] -> IO ()
locateEvents step t1 archive samples = do
    step "readback"
    readback <- do
        (a, _b) <- limits archive
        let player = mkPlayer archive Forward a
        result <- runSafeT $ PP.toListM player
        return result
    assertEqual "readback" samples (fmap snd readback)

    step "lookup UTC before"
    do
        result <- findEventByTimeUtc @() archive t1
        assertEqual "check" Nothing result

    step "lookup UTC exact"
    forM_ samples $ \event -> do
        let t = eTimeUtc event

        Just (ix, y) <- findEventByTimeUtc @() archive t
        y2 <- peekItem archive ix
        assertEqual "readback event1" event y
        assertEqual "readback event2" event y2

    step "lookup UTC between"
    forM_ (zip samples $ drop 1 samples) $ \(e1, e2) -> do
        let a = eTimeUtc e1
            b = eTimeUtc e2
            dt = diffUTCTime b a
            t = addUTCTime (dt/2) a
        when (t >= b) $ fail "Internal error, wrong sample"

        Just (_ix, y) <- findEventByTimeUtc @() archive t
        assertEqual "readback event" e2 y

    step "lookup UTC after"
    do
        let event = last samples
            t = addUTCTime 1 $ eTimeUtc event
        result <- findEventByTimeUtc @() archive t
        assertEqual "check" Nothing result

locateEventsInFile :: TestTree
locateEventsInFile = testCaseSteps "locate event in file" $ \step -> do
    forM_ [minBound..maxBound] $ \(enc :: FileEncoding) -> do
        step $ "encoding " <> show enc
        withSystemTempDirectory "vcr-test" $ \base' -> do
            let base = base' </> "rec"

            step "create random recording"

            -- Just to make sure to have positive utc + some margin.
            t1 <- generate arbitrary
            let t2 = addUTCTime (24*3600) t1
            samples :: [Event ()] <- do
                gen <- mkEventGenerator t2
                reverse . snd <$> saveFile 3000 enc (gen,[]) base

            let archive :: FileArchive
                archive = FileArchive enc base

            locateEvents step t1 archive samples

locateEventsInDirectory :: TestTree
locateEventsInDirectory = testCaseSteps "locate event in directory" $ \step -> do
    forM_ [minBound..maxBound] $ \(enc :: FileEncoding) -> do
        step $ "encoding " <> show enc
        withSystemTempDirectory "vcr-test" $ \base' -> do
            let numFiles = 10
                base = base' </> "rec"

            step "create random recording"

            -- Just to make sure to have positive utc + some margin.
            t1 <- generate arbitrary
            let t2 = addUTCTime (24*3600) t1
            samples :: [Event ()] <- do
                gen <- mkEventGenerator t2
                t0 <- generate arbitrary
                let tRotate = take numFiles $ iterate (addUTCTime 3600) t0
                    files = fileSuffixToFileName base . utcToFileSuffix <$> tRotate
                reverse . snd <$> foldM (saveFile 100 enc) (gen,[]) files

            let archive :: DirectoryArchive
                archive = DirectoryArchive enc base

            locateEvents step t1 archive samples

propTests :: TestTree
propTests = testGroup "Property tests"
    [ fileSuffixP
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $
    [ rawVcr "file" $ \base enc -> FileArchive enc (base </> "recording")
    , rawVcr "directory no-rotate" $ \base enc ->
        (DirectoryArchive enc (base </> "rec"), Rotate Nothing Nothing Nothing)
    , locateEventsInFile
    , locateEventsInDirectory
    ]
    ++ [ dirRotate n | n <- [1..5]]

tests :: TestTree
tests = testGroup "DiskTest" [propTests, unitTests]

main :: IO ()
main = defaultMain tests

