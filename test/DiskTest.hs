{-# LANGUAGE OverloadedStrings #-}

-- | Disk streaming unit and property tests.

module DiskTest where

import           Control.Monad
import           System.IO.Temp
import           System.FilePath
import           System.Random
import           System.IO
import           Data.Time
import           Data.Aeson
import qualified Data.ByteString as BS

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

limitsP :: TestTree
limitsP = QC.testProperty "file limits" $ \(enc::FileEncoding) -> QC.ioProperty $ do
    sizes <- generate $ listOf1 $ choose (1, 2*chunkSize)
    withSystemTempDirectory "vcr-test" $ \base -> do
        let f = base </> "rec"
        lst <- withFile f WriteMode $ \h -> do
            forM sizes $ \n -> do
                let s = BS.pack $ replicate n 255
                writeBytes enc h s
                return s

        let lst' = init lst
            x = fromIntegral (sum (fmap BS.length lst'))
                + fromIntegral ((length lst' * delimiterSize enc))

        (o1, o2) <- getFileLimits enc f
        assertEqual "offset1" 0 o1
        assertEqual "offset2" x o2

vcrEvents :: Ord ix =>
    TestName
    -> (FilePath -> FileEncoding -> Recorder (SafeT IO) (Event ()) ())
    -> (FilePath -> FileEncoding -> Player (SafeT IO) ix (Event ()))
    -> TestTree
vcrEvents title mkRec mkPl = testCaseSteps title $ \step -> do
    forM_ [minBound..maxBound] $ \(enc :: FileEncoding) -> do
        samples :: [Event ()] <- do
            n <- getPositive <$> generate arbitrary
            t0 <- generate arbitrary
            generate $ take n <$> (genEvents ["ch1", "ch2"] 100 t0)
        step $ "gen"
            <> ", events: " <> show (length samples)
        withSystemTempDirectory "vcr-test" $ \base -> do
            let logger _ = return ()
                recorder = mkRec base enc
                player = mkPl base enc

            step "create recording"
            do
                runSafeT $ runEffect (each samples >-> recorder logger)

            step "replay forward/backward"
            readback <- do
                (ix1, ix2) <- runSafeT $ limits player

                result1 <- runSafeT $ PP.toListM $ runPlayer player Forward ix1 Nothing
                assertEqual "complete" (length samples) (length result1)

                result2 <- runSafeT $ PP.toListM $ runPlayer player Backward ix2 Nothing
                assertBool "reversed" (result1 == reverse result2)

                forM_ (zip samples result1) $ \(x, (i, y)) -> do
                    assertBool "index" (i >= ix1)
                    assertBool "index" (i <= ix2)
                    assertEqual "readback" x y

                return result1

            step "replay from any valid index"
            do
                let validIndices = fmap fst readback

                step "forward"
                forM_ (zip [0..] validIndices) $ \(n,ix) -> do
                    let expected = drop n readback
                    result <- runSafeT $ PP.toListM $ runPlayer player Forward ix Nothing
                    assertBool "same result" (expected == result)

                step "backward"
                forM_ (zip [0..] validIndices) $ \(n,ix) -> do
                    let expected = reverse $ take (succ n) readback
                    result <- runSafeT $ PP.toListM $ runPlayer player Backward ix Nothing
                    assertBool "same result" (expected == result)

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

-- | Helper function to find events in any archive.
locateEvents :: (String -> IO ()) -> UtcTime
    -> Player (SafeT IO) Index (Event ()) -> [Event ()] -> IO ()
locateEvents step t1 player samples = do
    step "readback"
    readback <- do
        (a, _b) <- runSafeT $ limits player
        let p = runPlayer player Forward a Nothing
        result <- runSafeT $ PP.toListM p
        return result
    assertEqual "readback" samples (fmap snd readback)

    step "lookup UTC before"
    do
        Just (_ix, y) <- runSafeT $ findEventByTimeUtc player t1
        assertEqual "check" (head samples) y

    step "lookup UTC exact"
    forM_ samples $ \event -> do
        let t = eTimeUtc event

        Just (ix, y) <- runSafeT $ findEventByTimeUtc player t
        y2 <- runSafeT $ peekItem player ix
        assertEqual "readback event1" event y
        assertEqual "readback event2" event y2

    step "lookup UTC between"
    forM_ (zip samples $ drop 1 samples) $ \(e1, e2) -> do
        let a = eTimeUtc e1
            b = eTimeUtc e2
            dt = diffUTCTime b a
            t = addUTCTime (dt/2) a
        when (t >= b) $ fail "Internal error, wrong sample"

        Just (_ix, y) <- runSafeT $ findEventByTimeUtc player t
        assertEqual "readback event" e2 y

    step "lookup UTC after"
    do
        let event = last samples
            t = addUTCTime 1 $ eTimeUtc event
        result <- runSafeT $ findEventByTimeUtc player t
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
                let prod = produceEvents ["ch1", "ch2"] 100 t2
                reverse . snd <$> saveFile 3000 enc (prod, []) base

            let player :: Player (SafeT IO) FileOffset (Event ())
                player = mkFilePlayer $ FileArchive enc base

            locateEvents step t1 (reindex player) samples

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
                let gen = produceEvents ["ch1", "ch2"] 100 t2
                t0 <- generate arbitrary
                let tRotate = take numFiles $ iterate (addUTCTime 3600) t0
                    files = fileSuffixToFileName base . utcToFileSuffix <$> tRotate
                reverse . snd <$> foldM (saveFile 100 enc) (gen,[]) files

            let player :: Player (SafeT IO) DirectoryIndex (Event ())
                player = mkDirectoryPlayer $ DirectoryArchive enc base

            locateEvents step t1 (reindex player) samples

dirRotate :: Int -> TestTree
dirRotate numFiles = testCaseSteps title $ \step -> do
    forM_ [minBound..maxBound] $ \(enc :: FileEncoding) -> do
        withSystemTempDirectory "vcr-test" $ \base' -> do
            let base = base' </> "rec"
                player :: Player (SafeT IO) DirectoryIndex (Event ())
                player = mkDirectoryPlayer $ DirectoryArchive enc base

            step "create random recording files"
            t0 <- generate arbitrary
            let tRotate = take numFiles $ iterate (addUTCTime 3600) t0
            samples <- forM tRotate $ \t -> do
                samples :: [Event ()] <- generate (listOf1 arbitrary)
                let fs = utcToFileSuffix t
                    f  = fileSuffixToFileName base fs
                withFile f WriteMode $ \h -> do
                    mapM_ (writeBytes enc h . encodeJSON) samples
                return samples

            step "readback"
            readback <- do
                (a, _b) <- runSafeT $ limits player
                runSafeT $ PP.toListM $ runPlayer player Forward a Nothing
            assertEqual "readback" (join samples) (fmap snd readback)

            step "replay from any valid index"
            do
                let validIndices = fmap fst readback

                step "forward"
                forM_ (zip [0..] validIndices) $ \(n,ix) -> do
                    let expected = drop n readback
                    result <- runSafeT $ PP.toListM $ runPlayer player Forward ix Nothing
                    assertBool "same result" (expected == result)

                step "backward"
                forM_ (zip [0..] validIndices) $ \(n,ix) -> do
                    let expected = reverse $ take (succ n) readback
                    result <- runSafeT $ PP.toListM $ runPlayer player Backward ix Nothing
                    assertBool "same result" (expected == result)
  where
    title = "read from " <> show numFiles <> " rotated file(s)"

propTests :: TestTree
propTests = testGroup "Property tests"
    [ fileSuffixP
    , limitsP
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $
    [ vcrEvents "file"
        (\base enc -> jsonRecorder $ mkFileRecorder buf $ FileArchive enc (base </> "recording"))
        (\base enc -> mkFilePlayer $ FileArchive enc (base </> "recording"))
    , vcrEvents "directory no-rotate"
        (\base enc -> jsonRecorder $ mkDirectoryRecorder buf
            (DirectoryArchive enc (base </> "rec"), Rotate Nothing Nothing Nothing))
        (\base enc -> mkDirectoryPlayer $ DirectoryArchive enc (base </> "rec"))
    , locateEventsInFile
    , locateEventsInDirectory
    ]
    ++ [ dirRotate n | n <- [1..5]]
  where
    buf = Buffering Nothing False

tests :: TestTree
tests = testGroup "DiskTest" [propTests, unitTests]

main :: IO ()
main = defaultMain tests
