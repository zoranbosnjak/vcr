------------------
-- |
-- Module: File
--
-- File reader/writer.
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module File where

import           GHC.Generics (Generic)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Void
import           Data.Maybe
import           Data.Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Control.Exception as Ex
import qualified System.IO as IO
import qualified Data.Time
import           Data.List (sort, break)
import           Data.Function ((&))
import           System.Directory
import           System.FilePath
import           Text.Printf
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as L

-- local imports
import           Common
import           Time

type MyParser = MP.Parsec Void String

-- | File rotate options (for File output).
data Rotate = Rotate
    { rotateKeep            :: Int          -- keep number of old files
    , rotateSizeMegaBytes   :: Maybe Double -- max size of a single file
    , rotateTimeHours       :: Maybe Double -- max age of a single file
    } deriving (Generic, Eq, Show)

instance ToJSON Rotate
instance FromJSON Rotate

data FileSuffix = FileSuffix
    { ixYear    :: Integer
    , ixMonth   :: Int
    , ixDay     :: Int
    , ixHour    :: Int
    , ixMinute  :: Int
    , ixSecond  :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON FileSuffix
instance FromJSON FileSuffix

instance Ord FileSuffix where
    compare e1 e2
        = compare  (ixYear e1)   (ixYear e2)
        <> compare (ixMonth e1)  (ixMonth e2)
        <> compare (ixDay e1)    (ixDay e2)
        <> compare (ixHour e1)   (ixHour e2)
        <> compare (ixMinute e1) (ixMinute e2)
        <> compare (ixSecond e1) (ixSecond e2)

-- | Global event index
type Offset = Integer
data EventIndex = EventIndex
    { eixSuffix :: FileSuffix
    , eixOffset :: Offset
    } deriving (Generic, Eq, Show)

instance ToJSON EventIndex
instance FromJSON EventIndex

instance Ord EventIndex where
    compare (EventIndex fs1 offset1) (EventIndex fs2 offset2) =
        compare fs1 fs2 <> compare offset1 offset2

utcToFileSuffix :: UtcTime -> FileSuffix
utcToFileSuffix utc = FileSuffix
    { ixYear    = year
    , ixMonth   = month
    , ixDay     = day
    , ixHour    = Data.Time.todHour tod
    , ixMinute  = Data.Time.todMin tod
    , ixSecond  = ceiling (Data.Time.todSec tod)
    }
  where
    (year, month, day) = Data.Time.toGregorian $ Data.Time.utctDay utc
    tod = Data.Time.timeToTimeOfDay $ Data.Time.utctDayTime utc

fileSuffixToFileName :: FilePath -> FileSuffix -> FilePath
fileSuffixToFileName base suf = base ++ "-" ++ printf "%d-%02d-%02dT%02d-%02d-%02d"
    (ixYear suf) (ixMonth suf) (ixDay suf)
    (ixHour suf) (ixMinute suf) (ixSecond suf)

-- | Get recording suffix from the filename.
getFileSuffix :: FilePath -> FilePath -> Maybe FileSuffix
getFileSuffix base filename = case MP.runParser p "" filename of
    Right a -> Just a
    _ -> Nothing
  where
    p = FileSuffix
        <$> (MPC.string base >> MPC.string "-" >> (L.decimal :: MyParser Integer))
        <*> (MPC.string "-" >> (L.decimal :: MyParser Int))
        <*> (MPC.string "-" >> (L.decimal :: MyParser Int))
        <*> (MPC.string "T" >> (L.decimal :: MyParser Int))
        <*> (MPC.string "-" >> (L.decimal :: MyParser Int))
        <*> (MPC.string "-" >> (L.decimal :: MyParser Int))

-- | Recognize a recording file by name.
isRecordingFile :: FilePath -> FilePath -> Bool
isRecordingFile base filename = maybe False (const True) $ getFileSuffix base filename

-- | Get current recording files.
getRecordingFiles :: FilePath -> IO [FilePath]
getRecordingFiles base = do
    listing <- listDirectory (takeDirectory base)
    return $ sort $ Prelude.filter (isRecordingFile $ takeFileName base) listing

-- | Get recording files suffixes.
getRecordingFileSuffixes:: FilePath -> IO [FileSuffix]
getRecordingFileSuffixes base = do
    listing <- listDirectory (takeDirectory base)
    return $ sort $ catMaybes $ fmap (getFileSuffix $ takeFileName base) listing

-- | File writer with 'rotation' support.
rotatingFileLineWriter :: ToJSON a =>
    (Priority -> String -> IO ())           -- log message
    -> STM (Maybe (FilePath, Maybe Rotate)) -- get configuration
    -> STM a                                -- get value to write
    -> IO ()
rotatingFileLineWriter logM getConfig fetchObject = do
    logM INFO "startup"
    idleState
  where

    getEvent cfg = atomically
        (fmap Left cfgChange `orElse` fmap Right fetchObject)
      where
        cfgChange = do
            newConfig <- getConfig
            when (newConfig == cfg) retry
            return newConfig

    idleState = do
        logM INFO "idle"
        fix $ \loop -> do
            getEvent Nothing >>= either (maybe loop recorderState) (const loop)

    cleanup base keep = do
        let dirName = takeDirectory base
        olds <- drop keep . reverse <$> getRecordingFiles base
        mapM_ removeFile (fmap (dirName </>) olds)
        forM_ olds $ \i -> do
            logM INFO $ "old file removed: " ++ i

    recorderState cfg@(base, mRotate) = fix $ \loop -> do
        maybe (return ()) (cleanup base) (rotateKeep <$> mRotate)
        nowUtc <- getUtcTime
        nowPosix <- getPOSIXTime
        let recFile = fileSuffixToFileName base (utcToFileSuffix nowUtc)
        logM INFO $ "recording to: " ++ show recFile ++ ", " ++ show mRotate
        result <- do
            let acquire = IO.openFile recFile IO.AppendMode
                release h = do
                    n <- IO.hFileSize h
                    IO.hClose h
                    when (n == 0) $ do
                        logM INFO $ "removing empty file " ++ show recFile
                        removeFile recFile
            Ex.bracket acquire release $ \h -> do
                recorder cfg (0::Integer) nowPosix h

        case result of
            Nothing -> loop             -- rotate file
            Just cfg' -> case cfg' of   -- config change
                Nothing -> idleState
                Just newCfg -> recorderState newCfg

    recorder cfg@(_base, mRotate) !n t0 h = getEvent (Just cfg) >>= \case
        Left cfg' -> return (Just cfg')
        Right event -> do
            t <- getPOSIXTime
            let str = encodeCompact event
                n' = n + (fromIntegral $ BSL.length str)
                fileAge = t - t0

                sizeLimit = case join (fmap rotateSizeMegaBytes mRotate) of
                    Nothing -> False
                    Just mb -> n' >= round (mb*1024*1024)

                timeLimit = case join (fmap rotateTimeHours mRotate) of
                    Nothing -> False
                    Just hours -> (round fileAge) >= ((round $ hours * 3600)::Integer)

            BSL8.hPutStrLn h str
            case (sizeLimit || timeLimit) of
                True -> return Nothing
                False -> recorder cfg n' t0 h

getStartIndex :: FilePath -> IO (Either String EventIndex)
getStartIndex base = getRecordingFileSuffixes base >>= \case
    [] -> return $ Left "no recording found"
    (fs:_) -> return $ Right $ EventIndex fs 0

-- | Find EventIndex, based on the given utc time.
-- Assume a newline '\n' as record delimiter.
getNextIndex ::
    FilePath
    -> (BS.ByteString -> Either String UtcTime)
    -> UtcTime
    -> IO (Either String EventIndex)
getNextIndex base checkLine utc = do
    lst <- getRecordingFileSuffixes base
    let candidates = catMaybes $ getCandidates lst
    checkCandidates candidates
  where
    fs = utcToFileSuffix utc
    getCandidates lst =
        let (a,b) = break (> fs) lst
            c1 = bool (Just $ last a) Nothing (a == [])
            c2 = bool (Just $ head b) Nothing (b == [])
        in [c1, c2]

    checkCandidates [] = return $ Left "recordings exhausted"
    checkCandidates (x:xs) = do
        (b, result) <- IO.withFile (fileSuffixToFileName base x) IO.ReadMode $ \h -> do
            IO.hSeek h IO.SeekFromEnd 0
            b <- IO.hTell h
            ix <- findIndex h 0 b
            return (b, ix)
        case result of
            Nothing -> checkCandidates xs
            Just ix -> case ix >= b of
                True -> checkCandidates xs
                False -> return $ Right $ EventIndex x ix

    -- skip all lines where a timestamp is less then required
    -- return first of the remaining lines
    findIndex h = fix $ \loop a b -> case a >= b of
        True -> return Nothing
        False -> probe a >>= \case
            (Left e0, _) -> fail e0     -- first read should be OK
            (Right t0, _) -> case t0 >= utc of
                True -> return $ Just a     -- got it
                False -> case a >= b of
                    True -> return Nothing  -- not found
                    False -> do
                        -- Jump to the middle of the interval.
                        -- This will most likely position the file handle
                        -- in the middle of the line. Two readlines might be necessary.
                        let half = (a + b) `div` 2
                        mMiddle <- probe half >>= \case
                            (Right _, _) -> return $ Just half
                            (Left _, i) -> return $ bool (Just i) Nothing (i >= b)
                        case mMiddle of
                            Nothing -> linearSearch a b
                            Just i -> probe i >>= \case
                                (Left e, _) -> fail e
                                (Right t, j) -> case compare t utc of
                                    EQ -> return $ Just i
                                    LT -> bool (return $ Just i) (loop j b) (j > a)
                                    GT -> bool (return $ Just i) (loop a j) (j < b)
      where
        probe ix = do
            IO.hSeek h IO.AbsoluteSeek ix
            line <- BS8.hGetLine h
            ix' <- IO.hTell h
            return (checkLine line, ix')

        linearSearch a b
            | a >= b = return $ Nothing
            | otherwise = probe a >>= \case
                (Left e, _) -> fail e
                (Right t, i) -> case t >= utc of
                    True -> return $ Just a
                    False -> linearSearch i b

-- | Stream events from given index.
-- When end of file is reached, switch to the next recording file.
-- Stop streaming when (optional) limit of the number of events is reached.
streamFrom :: FilePath -> (BS.ByteString -> Maybe EventIndex -> IO ()) -> EventIndex -> Maybe Integer -> IO ()
streamFrom base processLine = fix $ \loop1 index mLimit1 -> do
    let filename = fileSuffixToFileName base (eixSuffix index)
    proceed <- IO.withFile filename IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek (eixOffset index)
        mLimit1 & (fix $ \loop2 mLimit2 -> do
            let limitReached = maybe False (<= 0) mLimit2
            case limitReached of
                True -> return Nothing
                False -> do
                    line <- BS8.hGetLine h
                    (mNextIndex, switchFile) <- IO.hIsEOF h >>= \case
                        False -> do
                            offset <- IO.hTell h
                            return (Just $ index { eixOffset = offset }, False)
                        True -> do
                            recordings <- drop 1 . dropWhile (/= (eixSuffix index)) <$> getRecordingFileSuffixes base
                            case recordings of
                                [] -> return (Nothing, True)
                                (nextSuffix:_) -> do
                                    return (Just $ EventIndex nextSuffix 0, True)
                    processLine line mNextIndex
                    let mLimit2' = fmap pred mLimit2
                    case mNextIndex of
                        Nothing -> return Nothing
                        Just nextIndex -> case switchFile of
                            False -> loop2 mLimit2'
                            True -> return $ Just (nextIndex, mLimit2')
            )
    case proceed of
        Nothing -> return ()
        Just (nextIndex, mLimit') -> loop1 nextIndex mLimit'

