
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCmdArchive (testCmdArchive) where

import System.IO
import System.IO.Temp
import qualified Data.ByteString as BS
import Data.List (sort)

import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Safe as PS

import Test.Tasty
import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit

import File
import Common as C
import Encodings
import Event
import Database
import CmdArchive as Cmd

testCmdArchive :: TestTree
testCmdArchive = testGroup "CmdArchive"
    [ testProperty "file -> file -> file" fileFileFile
    , testProperty "file -> db -> file" fileDbFile
    -- TODO: check filters
    ]

vcrOpts :: VcrOptions
vcrOpts = VcrOptions
    { vcrOptVerbose     = Nothing
    , vcrOptSyslog      = Nothing
    , vcrOptEkg         = Nothing
    }

opts :: Positive Int -> Positive Int -> Input -> Output -> Options
opts chunkBytes chunkEvents src dst = Cmd.Options
    { optInput          = src
    , optOutput         = dst
    , optChunkBytes     = getPositive chunkBytes
    , optChunkEvents    = getPositive chunkEvents
    , optMaxEventSize   = maxBound
    , optChunkDelay     = 0
    , optEventDelay     = 0
    , optChannelFilter  = []
    , optSourceIdFilter = []
    , optStartTime      = Nothing
    , optEndTime        = Nothing
    , optIgnoreError    = True      -- TODO: change arbitrary events
                                    -- to a random flow, such that sequences
                                    -- are correct and ignore errors flag is not
                                    -- necessary any more.
    }

-- | Generate temporary random data to a file and run action on it.
withTestData :: NonNegative Int -> EncodeFormat -> String
    -> ([Event] -> FilePath -> IO a) -> IO a
withTestData size enc fn act = withSystemTempFile fn $ \f h -> do
    hClose h
    orig :: [Event] <-
        Prelude.take (getNonNegative size) <$> generate arbitrary
    PS.runSafeT $ runEffect $
        each orig
        >-> PP.map (encode enc)
        >-> rotatingFileWriter (streamPath f) Nothing (\_ -> return ())
    act orig f

-- | Greate temp file and run action on it.
withTempFile1 :: FilePath -> (FilePath -> IO a) -> IO a
withTempFile1 fn act = withSystemTempFile fn $ \test h -> do
    hClose h
    act test

-- | Greate 2 temp files and run action on them.
withTempFile2 :: FilePath -> FilePath -> (FilePath -> FilePath -> IO a) -> IO a
withTempFile2 fn1 fn2 act =
    withTempFile1 fn1 $ \test1 ->
        withTempFile1 fn2 $ \test2 ->
            act test1 test2

fileFileFile :: Positive Int -> Positive Int -> NonNegative Int
    -> EncodeFormat -> EncodeFormat -> Property
fileFileFile chunkBytes chunkEvents totalSize encA encB = ioProperty $
    withTestData totalSize encA "test1" $ \orig test1 -> do
        withTempFile2 "test2" "test3" $ \test2 test3 -> do

            -- run archive: test1 -> test2
            flip Cmd.runCmd vcrOpts $ opts chunkBytes chunkEvents
                (IFile encA $ File.FileStore test1)
                (OFile encB $ File.FileStore test2)

            -- run archive: test2 -> test3
            flip Cmd.runCmd vcrOpts $ opts chunkBytes chunkEvents
                (IFile encB $ File.FileStore test2)
                (OFile encA $ File.FileStore test3)

            -- compare file contents
            cont1 <- BS.readFile test1
            cont2 <- BS.readFile test2
            cont3 <- BS.readFile test3
            let lstOrig = sort orig
                lst1 = sort <$> tryDecodeList encA cont1
                lst2 = sort <$> tryDecodeList encB cont2
                lst3 = sort <$> tryDecodeList encA cont3
            return $ conjoin
                [ lst1 === Just lstOrig
                , encA == encB =?> cont1 === cont2
                , lst2 === lst1
                , cont1 === cont3
                , lst3 === lst1
                ]

fileDbFile :: Positive Int -> Positive Int -> NonNegative Int
    -> EncodeFormat -> Property
fileDbFile chunkBytes chunkEvents totalSize enc = ioProperty $
    withTestData totalSize enc "test1" $ \orig test1 -> do
        withTempFile2 "test2" "test3" $ \test2 test3 -> do
            let db = DbSQLite3 test2
            prepareDatabase db

            -- run archive: test1 -> test2 (db)
            flip Cmd.runCmd vcrOpts $ opts chunkBytes chunkEvents
                (IFile enc $ File.FileStore test1)
                (ODatabase db)

            -- run archive: test2 (db) -> test3
            flip Cmd.runCmd vcrOpts $ opts chunkBytes chunkEvents
                (IDatabase db)
                (OFile enc $ File.FileStore test3)

            -- compare file content test1 and test3, expect equal
            cont1 <- BS.readFile test1
            cont3 <- BS.readFile test3
            let lstOrig = sort orig
                lst1 = sort <$> tryDecodeList enc cont1
                lst3 = sort <$> tryDecodeList enc cont3
            return $ conjoin
                [ lst1 === Just lstOrig
                , lst3 === lst1
                ]

-- | Like '==>', but 'skip' test, instead of discard.
infixr 0 =?>
(=?>) :: Bool -> Property -> Property
False =?> _ = property True
True  =?> p = p

