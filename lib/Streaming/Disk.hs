{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Streaming.Disk where

import           GHC.Generics (Generic)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import           Data.Aeson
import           Data.Void
import qualified Data.Aeson.Types as AT
import qualified System.IO as IO
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.ByteString.Lazy as BSL
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
import           Pipes.Safe
import qualified Pipes.Prelude as PP

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

delimiter :: FileEncoding -> BS.ByteString
delimiter = \case
    TextEncoding -> "\n"

delimiterSize :: Integral n => FileEncoding -> n
delimiterSize = fromIntegral . BS.length . delimiter

-- | File Streaming

data FileArchive a = FileArchive FileEncoding FilePath

instance IsRecorder (FileArchive BS.ByteString) (SafeT IO) where
    type RecItem (FileArchive BS.ByteString) = BS.ByteString

    mkRecorder (FileArchive enc path) _logM =
        bracket (IO.openFile path IO.AppendMode) IO.hClose $ \h ->
            forever $ do
                s <- await
                liftIO $ writeBytes enc h s

instance {-# OVERLAPPABLE #-} (ToJSON a) => IsRecorder (FileArchive a) (SafeT IO) where
    type RecItem (FileArchive a) = a

    mkRecorder (FileArchive enc path) logM =
        PP.map (BSL.toStrict . Data.Aeson.encode) >-> mkRecorder (FileArchive enc path) logM

getFileLimits :: FilePath -> IO (Maybe (FileOffset, FileOffset))
getFileLimits path = IO.withFile path IO.ReadMode $ \h -> do
    size <- IO.hFileSize h
    case size of
        0 -> return Nothing
        _ -> return $ Just (0, size)

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

-- | Decode JSON object, fail on error.
decodeJSON :: FromJSON a => BS.ByteString -> a
decodeJSON s = case decodeStrict s of
    Nothing -> error $ "Can not decode line: " ++ show s
    Just val -> val

instance MonadIO m => IsPlayer (FileArchive BS.ByteString) m FileOffset where
    type Item (FileArchive BS.ByteString) = BS.ByteString

    limits (FileArchive _enc path) = liftIO $ getFileLimits path
    middle (FileArchive enc path) a b = liftIO $ getFileMiddle enc path a b
    peekItem (FileArchive enc path) ix = liftIO $ readBytesAt enc path ix

instance {-# OVERLAPPABLE #-} (MonadIO m, FromJSON a) => IsPlayer (FileArchive a) m FileOffset where
    type Item (FileArchive a) = a

    limits (FileArchive _enc path) = liftIO $ getFileLimits path
    middle (FileArchive enc path) a b = liftIO $ getFileMiddle enc path a b
    peekItem (FileArchive enc path) ix = liftIO (decodeJSON <$> readBytesAt enc path ix)

-- | Stream data from the file.
playFile :: FileEncoding -> FilePath -> Direction -> FileOffset
    -> Producer (FileOffset, BS.ByteString) (SafeT IO) FileOffset
playFile enc path direction ix0 = do
    bracket (IO.openFile path IO.ReadMode) IO.hClose $ \h -> do
        size <- liftIO $ IO.hFileSize h
        liftIO $ IO.hSeek h IO.AbsoluteSeek ix0
        case direction of
            -- stepping backward over the file
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
                            | ixRead < 0 -> fail "negative read index value"
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
                            Nothing -> return 0
                            Just acc' -> do
                                let val = last acc'
                                    ixYield' = ixYield - fromIntegral (BS.length val) - delimiterSize enc
                                yield (ixYield', last acc')
                                loop ixYield' (init acc')

                loop ix0 []

            -- stepping forward over the file
            Forward -> fix $ \loop -> do
                ix <- liftIO $ IO.hTell h
                case compare ix size of
                    LT -> do
                        s <- liftIO $ readBytes enc h
                        yield (ix, s)
                        loop
                    _ -> return ix

instance IsRawPlayer (FileArchive BS.ByteString) (SafeT IO) FileOffset where
    mkRawPlayer (FileArchive enc path) direction ix = playFile enc path direction ix

instance {-# OVERLAPPABLE #-} FromJSON a => IsRawPlayer (FileArchive a) (SafeT IO) FileOffset where
    mkRawPlayer (FileArchive enc path) direction ix =
        mkRawPlayer (FileArchive enc path) direction ix >-> PP.map (\(i, val) -> (i, decodeJSON val))

-- | Directory Streaming.

data Rotate = Rotate
    { rotateKeep            :: Int          -- keep number of old files
    , rotateSizeMegaBytes   :: Maybe Double -- max size of a single file
    , rotateTimeHours       :: Maybe Double -- max age of a single file
    } deriving (Generic, Eq, Show)

instance ToJSON Rotate
instance FromJSON Rotate

data DirectoryArchive a = DirectoryArchive FileEncoding FilePath

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

instance ToJSON FileSuffix where
    toJSON (FileSuffix year month day hour minute second) =
        toJSON (year, month, day, hour, minute, second)

instance FromJSON FileSuffix where
    parseJSON = withArray "FilxSuffix" $ \v -> case V.length v of
        6 ->
            let f :: FromJSON a => Int -> AT.Parser a
                f n = parseJSON (v V.! n)
            in FileSuffix <$> f 0 <*> f 1 <*> f 2 <*> f 3 <*> f 4 <*> f 5
        _ -> mzero

data DirectoryIndex = DirectoryIndex
    { ixSuffix :: FileSuffix
    , ixOffset :: FileOffset
    } deriving (Generic, Eq, Show)

instance Ord DirectoryIndex where
    compare (DirectoryIndex fs1 offset1) (DirectoryIndex fs2 offset2) =
        compare fs1 fs2 <> compare offset1 offset2

instance IsRecorder (DirectoryArchive BS.ByteString, Maybe Rotate) (SafeT IO) where
    type RecItem (DirectoryArchive BS.ByteString, Maybe Rotate) = BS.ByteString

    mkRecorder (DirectoryArchive enc base, mRotate) logM = forever $ do
        -- cleanup directory
        case fmap rotateKeep mRotate of
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
                    sizeLimit = case join (fmap rotateSizeMegaBytes mRotate) of
                        Nothing -> False
                        Just mb -> n' >= round (mb*1024*1024)

                    timeLimit = case join (fmap rotateTimeHours mRotate) of
                        Nothing -> False
                        Just hours -> (round fileAge) >= ((round $ hours * 3600)::Integer)
                liftIO $ writeBytes enc h s
                case (sizeLimit || timeLimit) of
                    True -> return ()
                    False -> recorder n' t0 h

        lift $ logM $ "recording to: "
            <> T.pack (show recFile)
            <> ", " <> T.pack (show mRotate)

        bracket
            acquireResource
            releaseResource
            (recorder (0::Integer) nowPosix)

instance {-# OVERLAPPABLE #-} (ToJSON a) => IsRecorder (DirectoryArchive a, Maybe Rotate) (SafeT IO) where
    type RecItem (DirectoryArchive a, Maybe Rotate) = a

    mkRecorder (DirectoryArchive enc base, mRotate) logM =
        PP.map (BSL.toStrict . Data.Aeson.encode) >-> mkRecorder (DirectoryArchive enc base, mRotate) logM

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

getDirLimits :: FilePath -> IO (Maybe (DirectoryIndex, DirectoryIndex))
getDirLimits base = runMaybeT $ do
    lst <- lift $ getRecordingFiles base
    guard (lst /= [])
    let a = head lst
        b = last lst
    fsA <- MaybeT (pure $ getFileSuffix base a)
    fsB <- MaybeT (pure $ getFileSuffix base b)
    (a1, _a2) <- MaybeT $ getFileLimits a
    (_b1, b2) <- MaybeT $ getFileLimits b
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
            [] -> fail "empty recording"            -- unexpected at this point
            (_:[]) -> fail "single recording file"  -- unexpected at this point
            (_a:_b:[]) -> do
                -- TODO: implement for 2 files
                undefined
            _ -> do
                let ix = length lst `div` 2
                    fs = lst !! ix
                    f = fileSuffixToFileName base fs
                Just (a,b) <- getFileLimits f
                i <- getFileMiddle enc f a b
                let result = DirectoryIndex fsA i
                when (result <= di1) $ fail "internal index error"
                when (result >= di2) $ fail "internal index error"
                return result

instance MonadIO m => IsPlayer (DirectoryArchive BS.ByteString) m DirectoryIndex where
    type Item (DirectoryArchive BS.ByteString) = BS.ByteString

    limits (DirectoryArchive _enc base) = liftIO $ getDirLimits base
    middle (DirectoryArchive enc base) a b = liftIO $ getDirMiddle enc base a b
    peekItem (DirectoryArchive enc base) (DirectoryIndex fs foffset) =
        peekItem (FileArchive enc $ fileSuffixToFileName base fs) foffset

instance {-# OVERLAPPABLE #-} (MonadIO m, FromJSON a) => IsPlayer (DirectoryArchive a) m DirectoryIndex where
    type Item (DirectoryArchive a) = a

    limits (DirectoryArchive _enc base) = liftIO $ getDirLimits base
    middle (DirectoryArchive enc base) a b = liftIO $ getDirMiddle enc base a b
    peekItem (DirectoryArchive enc base) (DirectoryIndex fs foffset) =
        peekItem (FileArchive enc $ fileSuffixToFileName base fs) foffset

instance IsRawPlayer (DirectoryArchive BS.ByteString) (SafeT IO) DirectoryIndex where
    mkRawPlayer (DirectoryArchive enc base) direction (DirectoryIndex fs i) = do
        let f = fileSuffixToFileName base fs
            go p = lift (next p) >>= \case
                Right ((i', a), p') -> do
                    yield (DirectoryIndex fs i', a)
                    go p'
                Left _ -> do
                    lst <- liftIO $ getRecordingFileSuffixes base
                    nextFile direction lst >>= \case
                        Left val -> return val
                        Right (f', i') -> go $ playFile enc f' direction i'
        go $ playFile enc f direction i
      where
        nextFile _direction _lst = undefined

instance {-# OVERLAPPABLE #-} FromJSON a => IsRawPlayer (DirectoryArchive a) (SafeT IO) DirectoryIndex where
    mkRawPlayer (DirectoryArchive enc base) direction ix =
        mkRawPlayer (DirectoryArchive enc base) direction ix >-> PP.map (\(i, val) -> (i, decodeJSON val))

