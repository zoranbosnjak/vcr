------------------
-- |
-- Module: File
--
-- File reader/writer.
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module File where

import           GHC.Generics (Generic)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Void
import           Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           System.Posix.Files
import qualified Control.Exception as Ex
import qualified System.IO as IO
import qualified Data.Time
import           Data.List (sort)
import           Data.Function ((&))
import           System.IO (withFile, IOMode(ReadMode))
import           System.IO.Error (isDoesNotExistError)
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
    , ixSecond  = round (Data.Time.todSec tod)
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

-- | File writer with 'rotation' support.
rotatingFileLineWriter ::
    (Priority -> String -> IO ())           -- log message
    -> STM (Maybe (FilePath, Maybe Rotate)) -- get configuration
    -> STM BS.ByteString                    -- get line
    -> IO ()
rotatingFileLineWriter logM getConfig fetchLine = do
    atomically getConfig >>= maybe drain go
  where
    getEvent cfg = atomically (fmap Left cfgChange `orElse` fmap Right fetchLine)
      where
        cfgChange = do
            newConfig <- getConfig
            when (newConfig == cfg) retry
            return newConfig

    drain = do
        logM INFO "idle"
        fix $ \loop -> do
            getEvent Nothing >>= either (maybe loop go) (const loop)

    go (base, mRotate) = do
        logM INFO $ "starting, base file: " ++ show base ++ ", rotate: " ++ show mRotate

        -- get file status
        st <- Ex.tryJust (guard . isDoesNotExistError) (getFileStatus base) >>= \case
            Right st -> return st
            Left _ -> do
                IO.openFile base IO.AppendMode >>= IO.hClose
                getFileStatus base
        let size = fromIntegral $ fileSize st
            access = accessTimeHiRes st
        result <- IO.withFile base IO.AppendMode $ \h -> do
            loop h (size::Integer) access
        maybe drain go result

      where

        cleanup keep = do
            let dirName = takeDirectory base
            olds <- drop keep . reverse <$> getRecordingFiles base
            mapM_ removeFile (fmap (dirName </>) olds)
            return olds

        loop h !currentSize lastRotateTime = getEvent (Just (base, mRotate)) >>= \case
            Left newCfg -> do
                logM INFO "config changed"
                IO.hClose h
                return newCfg
            Right value -> do
                (nowUtc, nowPosix) <- (,) <$> getUtcTime <*> getPOSIXTime
                BS8.hPutStrLn h value
                let newSize = currentSize + (fromIntegral $ BS.length value)
                    rotateTo = do
                        rot <- mRotate
                        let sizeLimit = case rotateSizeMegaBytes rot of
                                Nothing -> False
                                Just mb -> newSize >= round (mb*1024*1024)
                            fileAge = nowPosix - lastRotateTime
                            timeLimit = case rotateTimeHours rot of
                                Nothing -> False
                                Just hours -> (round fileAge) >= ((round $ hours * 3600)::Integer)
                        let tFormat = Data.Time.formatTime Data.Time.defaultTimeLocale
                                (Data.Time.iso8601DateFormat (Just "%H-%M-%S"))
                        guard $ sizeLimit || timeLimit
                        Just (base ++ "-" ++ tFormat nowUtc, rotateKeep rot)
                case rotateTo of
                    Nothing -> loop h newSize lastRotateTime
                    Just (newName, keep) -> do
                        Ex.mask_ $ do
                            IO.hClose h
                            renameFile base newName
                        removed <- cleanup keep
                        logM INFO $ "output file rotated: " ++ newName
                        forM_ removed $ \i -> do
                            logM INFO $ "old file removed: " ++ i
                        return $ Just (base, mRotate)

getStartIndex :: FilePath -> IO (Either String EventIndex)
getStartIndex base = getRecordingFileSuffixes base >>= \case
    [] -> return $ Left "no recording found"
    (fs:_) -> return $ Right $ EventIndex fs 0

-- | Find EventIndex, based on the given utc time.
-- First locate the recording file, then use bisection to narrow the search.
-- Finally, use linear search to locate the file offset.
-- Assume a newline '\n' as record delimiter.
getNextIndex :: FilePath
    -> (BS.ByteString -> Either String UtcTime)
    -> UtcTime
    -> IO (Either String EventIndex)
getNextIndex base checkLine utc = getRecordingFileSuffixes base >>= \case
    [] -> return $ Left "no recording found"
    lst -> do
        let fs = utcToFileSuffix utc
            valid = dropWhile (< fs) lst
        case valid of
            [] -> return $ Left "recordings exhausted"
            (x:_xs) -> withFile (fileSuffixToFileName base x) ReadMode $ \h -> do
                let foundAt = Right . EventIndex x
                IO.hSeek h IO.SeekFromEnd 0
                b <- IO.hTell h
                bisect foundAt h 0 b
  where
    -- first narrow down search interval with bisection
    bisect foundAt h = fix $ \loop a b -> do
        -- This seek will most likely position the file handle
        -- in the middle of the line. Two readlines might be necessary.
        let half = (a + b) `div` 2
        IO.hSeek h IO.AbsoluteSeek half

        line <- BS8.hGetLine h
        fmap (>= b) (IO.hTell h ) >>= \case
            -- interval is already small enough, switch to linear search
            True -> do
                IO.hSeek h IO.AbsoluteSeek a
                linear foundAt h b
            -- bisect some more
            False -> do
                (t,j) <- case checkLine line of
                    Right t -> return (t, half)
                    Left _ -> do
                        j <- IO.hTell h
                        fmap checkLine (BS8.hGetLine h) >>= \case
                            Left e -> fail e
                            Right t -> return (t, j)
                i <- IO.hTell h
                case compare t utc of
                    EQ -> return $ foundAt j
                    LT -> loop i b
                    GT -> loop a j

    -- finally use linear search to find the offset
    linear foundAt h limit = fix $ \loop -> do
        i <- IO.hTell h
        case i >= limit of
            True -> return $ Left "search exhausted"
            False -> do
                t <- fmap checkLine (BS8.hGetLine h) >>= either fail pure
                case t >= utc of
                    True -> return $ foundAt i
                    False -> loop

