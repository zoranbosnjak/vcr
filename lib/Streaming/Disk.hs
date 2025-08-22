{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements recording and replay to/from disk.

module Streaming.Disk where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Aeson
import qualified Data.Aeson.Types              as AT
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.List                     (sort)
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Time
import qualified Data.Vector                   as V
import           Data.Void
import           GHC.Generics                  (Generic)
import           System.Directory
import           System.FilePath
import qualified System.IO                     as IO
import           Test.QuickCheck
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Printf
import qualified Text.Read                     as TR
import           Text.Read                     (readPrec)

import           Pipes
import qualified Pipes.Prelude                 as PP
import           Pipes.Safe

-- local imports
import           Time
import           Vcr

data Buffering = Buffering
    { bFileMode       :: Maybe IO.BufferMode
    , bFlushEachEvent :: Bool
    }

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

mkFileRecorder :: Buffering -> FileArchive -> Recorder (SafeT IO) BS.ByteString r
mkFileRecorder buf (FileArchive enc path) _logM =
    bracket acquireResource IO.hClose $ \h ->
        forever $ do
            s <- await
            liftIO $ writeEvent h s
  where
    acquireResource = do
        h <- IO.openFile path IO.AppendMode
        maybe (pure ()) (IO.hSetBuffering h) (bFileMode buf)
        pure h
    writeEvent h s = case bFlushEachEvent buf of
        False -> writeBytes enc h s
        True -> do
            writeBytes enc h s
            IO.hFlush h

-- | Calculate first and last valid index in a file.
getFileLimits :: FileEncoding -> FilePath -> IO (FileOffset, FileOffset)
getFileLimits enc path = IO.withFile path IO.ReadMode $ \h -> do
    size <- IO.hFileSize h
    ix <- go h size $ max 0 (size - chunkSize)
    pure (0, ix)
  where
    go h size ix = do
        IO.hSeek h IO.AbsoluteSeek ix
        s <- BS.hGetNonBlocking h (fromIntegral (size - ix))
        let events = splitBytes enc s
        case length events <= 1 of
            True -> case ix <= 0 of
                True  -> pure 0
                False -> go h size $ max 0 (ix - chunkSize)
            False -> do
                let n = fromIntegral $ BS.length $ last events
                pure $ size - n - delimiterSize enc

-- | Get approx. middle between 2 file offsets.
getFileMiddle :: FileEncoding -> FilePath -> FileOffset -> FileOffset -> IO FileOffset
getFileMiddle enc path a b = IO.withFile path IO.ReadMode $ \h -> do
    let half = (a + b) `div` 2
    IO.hSeek h IO.AbsoluteSeek half
    void $ readBytes enc h  -- first line is likely unusable
    ix1 <- IO.hTell h
    if
        | ix1 < b -> pure ix1
        | otherwise -> do
            IO.hSeek h IO.AbsoluteSeek a
            void $ readBytes enc h  -- skip line
            ix2 <- IO.hTell h
            if
                | ix2 < b   -> pure ix2 -- got it
                | otherwise -> pure a -- not found (return first limit)

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
            -- traverse the file forward
            Forward -> fix $ \loop -> do
                ix <- liftIO $ IO.hTell h
                case compare ix size of
                    LT -> do
                        s <- liftIO $ readBytes enc h
                        yield (ix, s)
                        loop
                    _ -> pure ()

            -- traverse the file backward
            -- Filesystem can only read forward efectively.
            -- Use accumulator, to reduce disk access.
            Backward -> do
                let fillAccumulator acc = do
                        ixRead <- liftIO $ IO.hTell h
                        if
                            -- no need to read more data
                            | length acc > 1 || (ixRead == 0 && length acc == 1)
                                -> pure $ Just acc
                            -- can not read any more
                            | ixRead < 0 -> throwM $ PlayError "negative read index value"
                            | ixRead == 0 -> pure Nothing
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
                            Nothing -> pure ()
                            Just acc' -> do
                                let val = last acc'
                                    ixYield' = ixYield - fromIntegral (BS.length val) - delimiterSize enc
                                yield (ixYield', last acc')
                                loop ixYield' (init acc')

                s <- liftIO $ readBytes enc h
                liftIO $ IO.hSeek h IO.AbsoluteSeek ix0
                yield (ix0, s)
                loop ix0 []

mkFilePlayer :: FromJSON a => FileArchive
    -> Player (SafeT IO) FileOffset (Event a)
mkFilePlayer (FileArchive enc path) = Player
    { limits = liftIO $ getFileLimits enc path
    , middle = \ix1 ix2 -> liftIO $ getFileMiddle enc path ix1 ix2
    , peekItem = \ix ->
        liftIO (readBytesAt enc path ix) >>= decodeJSON
    , runPlayer = \direction ix flt ->
        playFile enc path direction ix
        >-> PP.mapM (\(i, s) -> do
            s' <- decodeJSON s
            pure (i, s'))
        >-> dashEvents' direction flt
    }

-- | Directory Streaming.

data Rotate = Rotate
    { rotateKeep          :: Maybe Int    -- remove old files, keep some files
    , rotateSizeMegaBytes :: Maybe Double -- max size of a single file
    , rotateTimeHours     :: Maybe Double -- max age of a single file
    } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data DirectoryArchive = DirectoryArchive FileEncoding FilePath
    deriving (Show)

data FileSuffix = FileSuffix
    { fsYear   :: Integer
    , fsMonth  :: Int
    , fsDay    :: Int
    , fsHour   :: Int
    , fsMinute :: Int
    , fsSecond :: Int
    } deriving (Generic, Eq)

instance Show FileSuffix where
    show fs =
        show (fsYear fs)
        ++ "-" ++ show (fsMonth fs)
        ++ "-" ++ show (fsDay fs)
        ++ "-" ++ show (fsHour fs)
        ++ "-" ++ show (fsMinute fs)
        ++ "-" ++ show (fsSecond fs)

readCh :: Char -> TR.ReadPrec ()
readCh expected = do
    actual <- TR.get
    when (actual /= expected) $ fail "no parse"

instance Read FileSuffix where
    readPrec = FileSuffix
        <$> readPrec
        <*> (readCh '-' *> readPrec)
        <*> (readCh '-' *> readPrec)
        <*> (readCh '-' *> readPrec)
        <*> (readCh '-' *> readPrec)
        <*> (readCh '-' *> readPrec)

instance Ord FileSuffix where
    compare e1 e2 = mconcat
        [f fsYear, f fsMonth, f fsDay, f fsHour, f fsMinute, f fsSecond]
      where
        f :: Ord a => (FileSuffix -> a) -> Ordering
        f attr = compare (attr e1) (attr e2)

data DirectoryIndex = DirectoryIndex
    { ixSuffix :: FileSuffix
    , ixOffset :: FileOffset
    } deriving (Generic, Eq)

instance Show DirectoryIndex where
    show (DirectoryIndex a b) = show a ++ ":" ++ show b

instance Read DirectoryIndex where
    readPrec = DirectoryIndex
        <$> readPrec
        <*> (readCh ':' *> readPrec)

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

instance IsIndex DirectoryIndex where
    toIndex (DirectoryIndex fs offset) = Index $ NE.fromList
        [ fsYear fs
        , toInteger $ fsMonth fs
        , toInteger $ fsDay fs
        , toInteger $ fsHour fs
        , toInteger $ fsMinute fs
        , toInteger $ fsSecond fs
        , offset
        ]
    fromIndex (Index lst) = DirectoryIndex
        ( FileSuffix
            (NE.head lst)
            (fromInteger $ lst NE.!! 1)
            (fromInteger $ lst NE.!! 2)
            (fromInteger $ lst NE.!! 3)
            (fromInteger $ lst NE.!! 4)
            (fromInteger $ lst NE.!! 5)
        )
        (lst NE.!! 6)

utcToFileSuffix :: UtcTime -> FileSuffix
utcToFileSuffix t = FileSuffix
    { fsYear    = year
    , fsMonth   = month
    , fsDay     = day
    , fsHour    = Data.Time.todHour tod
    , fsMinute  = Data.Time.todMin tod
    , fsSecond  = ceiling (Data.Time.todSec tod)
    }
  where
    (year, month, day) = Data.Time.toGregorian $ Data.Time.utctDay t
    tod = Data.Time.timeToTimeOfDay $ Data.Time.utctDayTime t

fileSuffixToFileName :: FilePath -> FileSuffix -> FilePath
fileSuffixToFileName base suf = base ++ "-" ++ printf "%d-%02d-%02dT%02d-%02d-%02d"
    (fsYear suf) (fsMonth suf) (fsDay suf)
    (fsHour suf) (fsMinute suf) (fsSecond suf)

-- | Get recording suffix from the filename.
getFileSuffix :: FilePath -> FilePath -> Maybe FileSuffix
getFileSuffix base filename = case MP.runParser p "" filename of
    Right a -> Just a
    _       -> Nothing
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
isRecordingFile base filename = isJust $ getFileSuffix base filename

-- | Get current recording files.
getRecordingFiles :: FilePath -> IO [FilePath]
getRecordingFiles base = do
    listing <- listDirectory (takeDirectory base)
    pure $ sort $ Prelude.filter (isRecordingFile $ takeFileName base) listing

-- | Get recording files suffixes.
getRecordingFileSuffixes:: FilePath -> IO [FileSuffix]
getRecordingFileSuffixes base = do
    listing <- listDirectory (takeDirectory base)
    pure $ sort $ mapMaybe (getFileSuffix $ takeFileName base) listing

getDirLimits :: FileEncoding -> FilePath -> IO (DirectoryIndex, DirectoryIndex)
getDirLimits enc base = do
    files <- getRecordingFiles base
    let dirName = takeDirectory base
        lst = fmap (dirName </>) files
    when (null lst) $ throwM $ IndexError "empty recording"
    let a = lst !! 0
        b = last lst
    fsA <- maybe (throwM $ IndexError $ "filesuffix " <> a) pure $ getFileSuffix base a
    fsB <- maybe (throwM $ IndexError $ "filesuffix " <> b) pure $ getFileSuffix base b
    (a1, _a2) <- getFileLimits enc a
    (_b1, b2) <- getFileLimits enc b
    pure
        ( DirectoryIndex fsA a1
        , DirectoryIndex fsB b2
        )

getDirMiddle :: FileEncoding -> FilePath -> DirectoryIndex -> DirectoryIndex -> IO DirectoryIndex
getDirMiddle enc base di1@(DirectoryIndex fsA i1) di2@(DirectoryIndex fsB i2)
    | fsA == fsB = do   -- same file
        i <- getFileMiddle enc (fileSuffixToFileName base fsA) i1 i2
        pure $ DirectoryIndex fsA i
    | otherwise = do    -- different files
        lst <- getRecordingFileSuffixes base
            >>= pure . filter (\fs -> fs >= fsA && fs <= fsB)
        case lst of
            [] -> throwM $ IndexError "empty recording" -- unexpected at this point
            [_] -> throwM $ IndexError "single recording file" -- unexpected at this point
            [fs1, fs2] -> do    -- 2 files
                when (fs1 >= fs2) $ throwM $ IndexError "internal index error"
                (a1, b1) <- getFileLimits enc $ fileSuffixToFileName base fs1
                (a2, b2) <- getFileLimits enc $ fileSuffixToFileName base fs2
                when (i1 < a1) $ throwM $ IndexError "internal index error"
                when (i1 > b1) $ throwM $ IndexError "internal index error"
                when (i2 < a2) $ throwM $ IndexError "internal index error"
                when (i2 > b2) $ throwM $ IndexError "internal index error"
                if
                    | i1 >= b1 && i2 <= a2 -> pure $ DirectoryIndex fs1 b1
                    | i1 >= b1             -> pure $ DirectoryIndex fs2 a2
                    | otherwise            -> pure $ DirectoryIndex fs1 b1
            _ -> do             -- more files
                let ix = length lst `div` 2
                    fs = lst !! ix
                    f = fileSuffixToFileName base fs
                (a, b) <- getFileLimits enc f
                i <- getFileMiddle enc f a b
                let result = DirectoryIndex fs i
                when (result <= di1) $ throwM $ IndexError "internal index error"
                when (result >= di2) $ throwM $ IndexError "internal index error"
                pure result

mkDirectoryRecorder :: Buffering -> (DirectoryArchive, Rotate) -> Recorder (SafeT IO) BS.ByteString r
mkDirectoryRecorder buf (DirectoryArchive enc base, rotate) logM = forever $ do
    -- cleanup directory
    case rotateKeep rotate of
        Nothing -> pure ()
        Just keep -> do
            let dirName = takeDirectory base
            olds <- drop keep . reverse <$> liftIO (getRecordingFiles base)
            mapM_ (liftIO . removeFile) (fmap (dirName </>) olds)
            forM_ olds $ \i -> do
                lift $ logM $ "old file removed: " <> T.pack (show i)

    -- create new recording file
    nowUtc <- liftIO getUtcTime
    nowPosix <- liftIO getPOSIXTime
    let recFile = fileSuffixToFileName base (utcToFileSuffix nowUtc)
        acquireResource = do
            h <- IO.openFile recFile IO.AppendMode
            maybe (pure ()) (IO.hSetBuffering h) (bFileMode buf)
            pure h
        releaseResource h = do
            n <- IO.hFileSize h
            IO.hClose h
            when (n == 0) $ do
                runSafeT $ logM $ "removing empty file " <> T.pack (show recFile)
                removeFile recFile
        writeEvent h s = case bFlushEachEvent buf of
            False -> writeBytes enc h s
            True -> do
                writeBytes enc h s
                IO.hFlush h

        -- recorder loop
        recorder !n t0 h = do
            s <- await
            t <- liftIO getPOSIXTime
            let n' = n + fromIntegral (BS.length s)
                fileAge = t - t0
                sizeLimit = case rotateSizeMegaBytes rotate of
                    Nothing -> False
                    Just mb -> n' >= round (mb*1024*1024)

                timeLimit = case rotateTimeHours rotate of
                    Nothing -> False
                    Just hours -> round fileAge >= ((round $ hours * 3600)::Integer)
            liftIO $ writeEvent h s
            case sizeLimit || timeLimit of
                True  -> pure ()
                False -> recorder n' t0 h

    lift $ logM $ "recording to: "
        <> T.pack (show recFile)
        <> ", " <> T.pack (show rotate)

    bracket
        acquireResource
        releaseResource
        (recorder (0::Integer) nowPosix)

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
                Nothing -> pure ()
                Just (fs', i') -> do
                    playDirectory enc base direction (DirectoryIndex fs' i')
    nextFile fsCurrent = do
        lst <- liftIO $ getRecordingFileSuffixes base
        let result = case direction of
                Forward  -> dropWhile (<= fsCurrent) lst
                Backward -> dropWhile (>= fsCurrent) (reverse lst)
        case result of
                [] -> pure Nothing
                (fs':_) -> do
                    let f' = fileSuffixToFileName base fs'
                    (a, b) <- liftIO $ getFileLimits enc f'
                    let ix = case direction of
                            Forward  -> a
                            Backward -> b
                    pure $ Just (fs', ix)

mkDirectoryPlayer :: FromJSON a => DirectoryArchive
    -> Player (SafeT IO) DirectoryIndex (Event a)
mkDirectoryPlayer (DirectoryArchive enc base) = Player
    { limits = liftIO $ getDirLimits enc base
    , middle = \ix1 ix2 -> liftIO $ getDirMiddle enc base ix1 ix2
    , peekItem = \(DirectoryIndex fs foffset) ->
        let fPlayer = mkFilePlayer (FileArchive enc $ fileSuffixToFileName base fs)
        in peekItem fPlayer foffset
    , runPlayer = \direction ix flt ->
        playDirectory enc base direction ix
        >-> PP.mapM (\(i, s) -> do
            s' <- decodeJSON s
            pure (i, s'))
        >-> dashEvents' direction flt
    }
