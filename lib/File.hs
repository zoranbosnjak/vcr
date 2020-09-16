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
import qualified Data.Vector as V
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
import           System.Directory
import           System.FilePath
import           Text.Printf
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as L

import           Pipes
import           Pipes.Safe

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

instance ToJSON FileSuffix where
    toJSON (FileSuffix year month day hour minute second) =
        toJSON (year, month, day, hour, minute, second)

instance FromJSON FileSuffix where
    parseJSON = withArray "FilxSuffix" $ \v -> case V.length v of
        6 ->
            let f n = parseJSON (v V.! n)
            in FileSuffix <$> f 0 <*> f 1 <*> f 2 <*> f 3 <*> f 4 <*> f 5
        _ -> mzero

instance Ord FileSuffix where
    compare e1 e2 = mconcat
        [f ixYear, f ixMonth, f ixDay, f ixHour, f ixMinute, f ixSecond]
      where
        f attr = compare (attr e1) (attr e2)

-- | Global (line) index
type Offset = Integer
data Index = Index
    { ixSuffix :: FileSuffix
    , ixOffset :: Offset
    } deriving (Generic, Eq, Show)

instance ToJSON Index where
    toJSON (Index a b) = toJSON (a,b)

instance FromJSON Index where
    parseJSON = withArray "Index" $ \v -> case V.length v of
        2 ->
            let f n = parseJSON (v V.! n)
            in Index <$> f 0 <*> f 1
        _ -> mzero

instance Ord Index where
    compare (Index fs1 offset1) (Index fs2 offset2) =
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
            let acquireResource = IO.openFile recFile IO.AppendMode
                releaseResource h = do
                    n <- IO.hFileSize h
                    IO.hClose h
                    when (n == 0) $ do
                        logM INFO $ "removing empty file " ++ show recFile
                        removeFile recFile
            Ex.bracket acquireResource releaseResource $ \h -> do
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

-- | Get the first index.
getStartIndex :: FilePath -> IO (Either String Index)
getStartIndex base = getRecordingFileSuffixes base >>= \case
    [] -> return $ Left "no recording found"
    (fs:_) -> return $ Right $ Index fs 0

-- | Find next Index, based on the given index.
-- Assume a newline '\n' as record delimiter.
getNextIndexFromIndex :: FilePath -> Index -> IO (Either String Index)
getNextIndexFromIndex base index = do
    (x:xs) <- dropWhile (< (ixSuffix index)) <$> getRecordingFileSuffixes base
    IO.withFile (fileSuffixToFileName base x) IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek $ ixOffset index
        void $ BS8.hGetLine h
        IO.hIsEOF h >>= \case
            False -> Right . Index x <$> IO.hTell h
            True -> case xs of
                [] -> return $ Left "recordings exhausted"
                (y:_) -> return $ Right $ Index y 0

-- | Find next Index, based on the given utc time.
-- Assume a newline '\n' as record delimiter.
getNextIndexFromUtc ::
    FilePath
    -> (BS.ByteString -> Either String UtcTime)
    -> UtcTime
    -> IO (Either String Index)
getNextIndexFromUtc base checkLine utc = do
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
            ix <- findIndex checkLine utc h 0 b
            return (b, ix)
        case result of
            Nothing -> checkCandidates xs
            Just ix -> case ix >= b of
                True -> checkCandidates xs
                False -> return $ Right $ Index x ix

-- skip all lines where a timestamp is less then required
-- return first of the remaining lines
findIndex :: Ord a =>
    (BS8.ByteString -> Either String a)
    -> a -> IO.Handle -> Integer -> Integer -> IO (Maybe Integer)
findIndex checkLine utc h = fix $ \loop a b -> case a >= b of
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
    probe offset = do
        IO.hSeek h IO.AbsoluteSeek offset
        line <- BS8.hGetLine h
        offset' <- IO.hTell h
        return (checkLine line, offset')

    linearSearch a b
        | a >= b = return $ Nothing
        | otherwise = probe a >>= \case
            (Left e, _) -> fail e
            (Right t, i) -> case t >= utc of
                True -> return $ Just a
                False -> linearSearch i b

-- | File reader.
-- Assume '\n' separator.
-- When end of file is reached, switch to the next recording file.
-- Stop streaming when no more data.
lineReader :: FilePath -> Index -> Producer (Index, BS.ByteString) (SafeT IO) ()
lineReader base = fix $ \loop startIndex -> do
    let filename = fileSuffixToFileName base (ixSuffix startIndex)
    proceed <- bracket (IO.openFile filename IO.ReadMode) IO.hClose $ \h -> do
        liftIO $ IO.hSeek h IO.AbsoluteSeek (ixOffset startIndex)
        streamFrom (ixSuffix startIndex) h
    maybe (return ()) loop proceed
  where
    streamFrom suffix h = fix $ \loop -> do
        index <- Index <$> pure suffix <*> liftIO (IO.hTell h)
        liftIO (IO.hIsEOF h) >>= \case
            True -> do
                recordings <- dropWhile (<= (ixSuffix index)) <$> liftIO (getRecordingFileSuffixes base)
                case recordings of
                    [] -> return Nothing
                    (nextSuffix:_) -> do
                        return $ Just $ Index nextSuffix 0
            False -> do
                line <- liftIO $ BS8.hGetLine h
                yield (index, line)
                loop

