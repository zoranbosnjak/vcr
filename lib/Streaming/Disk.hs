{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Streaming.Disk where

import           GHC.Generics (Generic)
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.Vector as V
import           Data.Aeson
import           Data.Void
import qualified Data.Aeson.Types as AT
import qualified System.IO as IO
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Time
import           Data.List (sort)
import           Data.Maybe
import           System.Directory
import           System.FilePath
import qualified Data.Text as T
import           Text.Printf
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as L
import           Test.QuickCheck

import           Pipes
import qualified Pipes.Prelude as PP
import           Pipes.Safe

-- local imports
import           Time
import           Vcr

chunkSize :: Integral n => n
chunkSize = max (fromIntegral defaultChunkSize) 10000

type FileOffset = Integer
type StrParser = MP.Parsec Void String

data FileEncoding
    = TextEncoding      -- '\n' termination
    -- other encodings possible, e.g. CobsEncoding, -- '\0' termination
    deriving (Generic, Eq, Show, Bounded, Enum)

instance Arbitrary FileEncoding where
    arbitrary = oneof (fmap pure [minBound..maxBound])

-- | Write some bytes and terminating character.
writeBytes :: FileEncoding -> IO.Handle -> BS.ByteString -> IO ()
writeBytes enc h s = case enc of
    TextEncoding -> BS8.hPutStrLn h s

-- | Read some bytes, consume terminating character.
readBytes :: FileEncoding -> IO.Handle -> IO BS.ByteString
readBytes enc h = case enc of
    TextEncoding -> BS8.hGetLine h

-- | Split bytestring.
splitBytes :: FileEncoding -> BS.ByteString -> [BS.ByteString]
splitBytes enc s = case enc of
    TextEncoding -> BS8.lines s

delimiter :: FileEncoding -> Char
delimiter = \case
    TextEncoding -> '\n'

delimiterSize :: Integral n => FileEncoding -> n
delimiterSize = \case
    TextEncoding -> 1

-- | File Streaming

data FileArchive = FileArchive FileEncoding FilePath

instance Recorder FileArchive (SafeT IO) BS.ByteString where
    mkRecorder (FileArchive enc path) _logM =
        bracket (IO.openFile path IO.AppendMode) IO.hClose $ \h ->
            forever $ do
                s <- await
                liftIO $ writeBytes enc h s

getFileLimits :: FilePath -> IO (FileOffset, FileOffset)
getFileLimits path = IO.withFile path IO.ReadMode $ \h -> do
    size <- IO.hFileSize h
    return (0, size)

-- | Get approx. middle between 2 file offsets.
getFileMiddle :: FileEncoding -> FilePath -> FileOffset -> FileOffset -> IO FileOffset
getFileMiddle enc path a b = IO.withFile path IO.ReadMode $ \h -> do
    let half = (a + b) `div` 2
    IO.hSeek h IO.AbsoluteSeek half
    void $ readBytes enc h  -- first line is likely unusable
    ix1 <- IO.hTell h
    if
        | ix1 < b -> return ix1
        | otherwise -> do
            IO.hSeek h IO.AbsoluteSeek a
            void $ readBytes enc h  -- skip line
            ix2 <- IO.hTell h
            if
                | ix2 < b -> return ix2 -- got it
                | otherwise -> return a -- not found (return first limit)

-- | Read some bytes from given index.
readBytesAt :: FileEncoding -> FilePath -> FileOffset -> IO BS.ByteString
readBytesAt enc path ix = IO.withFile path IO.ReadMode $ \h -> do
    IO.hSeek h IO.AbsoluteSeek ix
    readBytes enc h

-- | Stream data from the file.
playFile :: FileEncoding -> FilePath -> Direction -> FileOffset
    -> Producer (FileOffset, BS.ByteString) (SafeT IO) ()
playFile enc path direction ix0 = do
    bracket (IO.openFile path IO.ReadMode) IO.hClose $ \h -> do
        size <- liftIO $ IO.hFileSize h
        liftIO $ IO.hSeek h IO.AbsoluteSeek ix0
        case direction of
            -- traverse the file backward
            -- Filesystem can only read forward efectively.
            -- Use accumulator, to reduce disk access.
            Backward -> do
                let fillAccumulator acc = do
                        ixRead <- liftIO $ IO.hTell h
                        if
                            -- no need to read more data
                            | length acc > 1 || (ixRead == 0 && length acc == 1)
                                -> return $ Just acc
                            -- can not read any more
                            | ixRead < 0 -> throwM $ PlayError "negative read index value"
                            | ixRead == 0 -> return Nothing
                            -- read more data
                            | otherwise -> do
                                let ixRead' = max 0 (ixRead - chunkSize)
                                liftIO $ IO.hSeek h IO.AbsoluteSeek ixRead'
                                s <- liftIO $ BS.hGet h $ fromIntegral (ixRead - ixRead')
                                liftIO $ IO.hSeek h IO.AbsoluteSeek ixRead'
                                let acc' = splitBytes enc (s <> mconcat acc)
                                fillAccumulator acc'

                let loop !ixYield acc = do
                        proceed <- fillAccumulator acc
                        case proceed of
                            Nothing -> return ()
                            Just acc' -> do
                                let val = last acc'
                                    ixYield' = ixYield - fromIntegral (BS.length val) - delimiterSize enc
                                yield (ixYield', last acc')
                                loop ixYield' (init acc')

                loop ix0 []

            -- traverse the file forward
            Forward -> fix $ \loop -> do
                ix <- liftIO $ IO.hTell h
                case compare ix size of
                    LT -> do
                        s <- liftIO $ readBytes enc h
                        yield (ix, s)
                        loop
                    _ -> return ()

instance (MonadIO m, MonadThrow m) => Indexed FileArchive m where
    type Index FileArchive = FileOffset

    limits (FileArchive _enc path) = liftIO $ getFileLimits path
    middle (FileArchive enc path) a b = liftIO $ getFileMiddle enc path a b

instance (MonadIO m, MonadThrow m) => HasItem FileArchive m BS.ByteString where
    peekItem (FileArchive enc path) ix = liftIO $ do
        readBytesAt enc path ix

instance (MonadIO m, MonadThrow m, FromJSON a) => HasItem FileArchive m (Event a) where
    peekItem (FileArchive enc path) ix = liftIO $ do
        readBytesAt enc path ix >>= decodeJSON

instance Player FileArchive (SafeT IO) BS.ByteString where
    mkPlayer (FileArchive enc path) direction ix =
        playFile enc path direction ix

instance FromJSON a => Player FileArchive (SafeT IO) (Event a) where
    mkPlayer (FileArchive enc path) direction ix =
        playFile enc path direction ix
        >-> PP.mapM (\(i, s) -> do
            s' <- decodeJSON s
            return (i, s'))

-- | Directory Streaming.

data Rotate = Rotate
    { rotateKeep            :: Maybe Int    -- remove old files, keep some files
    , rotateSizeMegaBytes   :: Maybe Double -- max size of a single file
    , rotateTimeHours       :: Maybe Double -- max age of a single file
    } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data DirectoryArchive = DirectoryArchive FileEncoding FilePath
    deriving (Show)

data FileSuffix = FileSuffix
    { ixYear    :: Integer
    , ixMonth   :: Int
    , ixDay     :: Int
    , ixHour    :: Int
    , ixMinute  :: Int
    , ixSecond  :: Int
    } deriving (Generic, Eq, Show)

instance Ord FileSuffix where
    compare e1 e2 = mconcat
        [f ixYear, f ixMonth, f ixDay, f ixHour, f ixMinute, f ixSecond]
      where
        f :: Ord a => (FileSuffix -> a) -> Ordering
        f attr = compare (attr e1) (attr e2)

data DirectoryIndex = DirectoryIndex
    { ixSuffix :: FileSuffix
    , ixOffset :: FileOffset
    } deriving (Generic, Eq, Show)

instance ToJSON DirectoryIndex where
    toJSON (DirectoryIndex (FileSuffix year month day hour minute second) offset) =
        toJSON (year, month, day, hour, minute, second, offset)

instance FromJSON DirectoryIndex where
    parseJSON = withArray "DirectoryIndex" $ \v -> case V.length v of
        7 ->
            let f :: FromJSON a => Int -> AT.Parser a
                f n = parseJSON (v V.! n)
            in DirectoryIndex
                <$> (FileSuffix <$> f 0 <*> f 1 <*> f 2 <*> f 3 <*> f 4 <*> f 5)
                <*> f 6
        _ -> mzero

instance Ord DirectoryIndex where
    compare (DirectoryIndex fs1 offset1) (DirectoryIndex fs2 offset2) =
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
        <$> (MPC.string base >> MPC.string "-" >> (L.decimal :: StrParser Integer))
        <*> (MPC.string "-" >> (L.decimal :: StrParser Int))
        <*> (MPC.string "-" >> (L.decimal :: StrParser Int))
        <*> (MPC.string "T" >> (L.decimal :: StrParser Int))
        <*> (MPC.string "-" >> (L.decimal :: StrParser Int))
        <*> (MPC.string "-" >> (L.decimal :: StrParser Int))

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

getDirLimits :: FilePath -> IO (DirectoryIndex, DirectoryIndex)
getDirLimits base = do
    files <- getRecordingFiles base
    let dirName = takeDirectory base
        lst = fmap (dirName </>) files
    when (lst == []) $ throwM $ IndexError "empty recording"
    let a = head lst
        b = last lst
    fsA <- maybe (throwM $ IndexError $ "filesuffix " <> a) pure $ getFileSuffix base a
    fsB <- maybe (throwM $ IndexError $ "filesuffix " <> b) pure $ getFileSuffix base b
    (a1, _a2) <- getFileLimits a
    (_b1, b2) <- getFileLimits b
    return
        ( DirectoryIndex fsA a1
        , DirectoryIndex fsB b2
        )

getDirMiddle :: FileEncoding -> FilePath -> DirectoryIndex -> DirectoryIndex -> IO DirectoryIndex
getDirMiddle enc base di1@(DirectoryIndex fsA i1) di2@(DirectoryIndex fsB i2)
    | fsA == fsB = do   -- same file
        i <- getFileMiddle enc (fileSuffixToFileName base fsA) i1 i2
        return $ DirectoryIndex fsA i
    | otherwise = do    -- different files
        lst <- getRecordingFileSuffixes base
            >>= return . filter (\fs -> fs >= fsA && fs <= fsB)
        case lst of
            [] -> throwM $ IndexError "empty recording" -- unexpected at this point
            (_:[]) -> throwM $ IndexError "single recording file" -- unexpected at this point
            (fs1:fs2:[]) -> do    -- 2 files
                when (fs1 >= fs2) $ throwM $ IndexError "internal index error"
                (a1, b1) <- getFileLimits $ fileSuffixToFileName base fs1
                (a2, b2) <- getFileLimits $ fileSuffixToFileName base fs2
                when (i1 < a1) $ throwM $ IndexError "internal index error"
                when (i1 >= b1) $ throwM $ IndexError "internal index error"
                when (i2 < a2) $ throwM $ IndexError "internal index error"
                when (i2 > b2) $ throwM $ IndexError "internal index error"
                case i2 > a2 of
                    True -> return $ DirectoryIndex fs2 a2  -- start of fs2
                    False -> do
                        let f = fileSuffixToFileName base fs1
                        i <- getFileMiddle enc f i1 b1
                        return $ DirectoryIndex fs1 i
            _ -> do             -- more files
                let ix = length lst `div` 2
                    fs = lst !! ix
                    f = fileSuffixToFileName base fs
                (a, b) <- getFileLimits f
                i <- getFileMiddle enc f a b
                let result = DirectoryIndex fs i
                when (result <= di1) $ throwM $ IndexError "internal index error"
                when (result >= di2) $ throwM $ IndexError "internal index error"
                return result

instance Recorder (DirectoryArchive, Rotate) (SafeT IO) BS.ByteString where
    mkRecorder (DirectoryArchive enc base, rotate) logM = forever $ do
        -- cleanup directory
        case rotateKeep rotate of
            Nothing -> return ()
            Just keep -> do
                let dirName = takeDirectory base
                olds <- drop keep . reverse <$> (liftIO $ getRecordingFiles base)
                mapM_ (liftIO . removeFile) (fmap (dirName </>) olds)
                forM_ olds $ \i -> do
                    lift $ logM $ "old file removed: " <> T.pack (show i)

        -- create new recording file
        nowUtc <- liftIO getUtcTime
        nowPosix <- liftIO getPOSIXTime
        let recFile = fileSuffixToFileName base (utcToFileSuffix nowUtc)
            acquireResource = IO.openFile recFile IO.AppendMode
            releaseResource h = do
                n <- IO.hFileSize h
                IO.hClose h
                when (n == 0) $ do
                    runSafeT $ logM $ "removing empty file " <> (T.pack $ show recFile)
                    removeFile recFile

            -- recorder loop
            recorder !n t0 h = do
                s <- await
                t <- liftIO getPOSIXTime
                let n' = n + (fromIntegral $ BS.length s)
                    fileAge = t - t0
                    sizeLimit = case rotateSizeMegaBytes rotate of
                        Nothing -> False
                        Just mb -> n' >= round (mb*1024*1024)

                    timeLimit = case rotateTimeHours rotate of
                        Nothing -> False
                        Just hours -> (round fileAge) >= ((round $ hours * 3600)::Integer)
                liftIO $ writeBytes enc h s
                case (sizeLimit || timeLimit) of
                    True -> return ()
                    False -> recorder n' t0 h

        lift $ logM $ "recording to: "
            <> T.pack (show recFile)
            <> ", " <> T.pack (show rotate)

        bracket
            acquireResource
            releaseResource
            (recorder (0::Integer) nowPosix)

-- | A 'DirectoryArchive' is normally paired with (_, Rotate).
-- Whatever is valid for 's', is also valid for (s, Rotate).

instance Indexed s m => Indexed (s, Rotate) m where
    type Index (s, Rotate) = Index s
    limits (s, _rot) = limits s
    middle (s, _rot) = middle s

instance HasItem s m a => HasItem (s, Rotate) m a where
    peekItem (s, _rot) = peekItem s

instance Player s m a => Player (s, Rotate) m a where
    mkPlayer (s, _rot) = mkPlayer s

instance PlayerF s m a => PlayerF (s, Rotate) m a where
    mkPlayerF (s, _rot) = mkPlayerF s

instance MonadIO m => Indexed DirectoryArchive m where
    type Index DirectoryArchive = DirectoryIndex

    limits (DirectoryArchive _enc base) = liftIO $ getDirLimits base
    middle (DirectoryArchive enc base) a b = liftIO $ getDirMiddle enc base a b

instance (MonadIO m, MonadThrow m) => HasItem DirectoryArchive m BS.ByteString where
    peekItem (DirectoryArchive enc base) (DirectoryIndex fs foffset) =
        peekItem (FileArchive enc $ fileSuffixToFileName base fs) foffset

instance (MonadIO m, MonadThrow m, FromJSON a) => HasItem DirectoryArchive m (Event a) where
    peekItem (DirectoryArchive enc base) (DirectoryIndex fs foffset) =
        peekItem (FileArchive enc $ fileSuffixToFileName base fs) foffset >>= decodeJSON

playDirectory :: FileEncoding -> FilePath -> Direction -> DirectoryIndex
    -> Producer (DirectoryIndex, BS.ByteString) (SafeT IO) ()
playDirectory enc base direction (DirectoryIndex fs i) = do
    let f = fileSuffixToFileName base fs
    go $ playFile enc f direction i
  where
    go p = lift (next p) >>= \case
        Right ((i', a), p') -> do
            yield (DirectoryIndex fs i', a)
            go p'
        Left () -> do
            nextFile fs >>= \case
                Nothing -> return ()
                Just (fs', i') -> do
                    playDirectory enc base direction (DirectoryIndex fs' i')
    nextFile fsCurrent = do
        lst <- liftIO $ getRecordingFileSuffixes base
        let result = case direction of
                Backward -> dropWhile (>= fsCurrent) (reverse lst)
                Forward -> dropWhile (<= fsCurrent) lst
        case result of
                [] -> return Nothing
                (fs':_) -> do
                    let f' = fileSuffixToFileName base fs'
                    (a, b) <- liftIO $ getFileLimits f'
                    let ix = case direction of
                            Backward -> b
                            Forward -> a
                    return $ Just (fs', ix)

instance Player DirectoryArchive (SafeT IO) BS.ByteString where
    mkPlayer (DirectoryArchive enc base) direction ix =
        playDirectory enc base direction ix

instance FromJSON a => Player DirectoryArchive (SafeT IO) (Event a) where
    mkPlayer (DirectoryArchive enc base) direction ix =
        playDirectory enc base direction ix
        >-> PP.mapM (\(i, s) -> do
            s' <- decodeJSON s
            return (i, s'))

