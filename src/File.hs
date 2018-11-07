------------------
-- |
-- Module: File
--
-- This module provides common File definitions.
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module File where

import           Control.Monad
import           Control.Exception as Ex
import           Control.Monad.Fix
import           Control.Concurrent.STM
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, withText)
import           Data.String
import qualified Options.Applicative as Opt
import           Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified System.IO as IO
import qualified Data.Time as Time
import           Data.List (isPrefixOf, sort)
import           Data.Time.Clock.POSIX
import           System.Directory (renameFile, listDirectory, removeFile)
import           System.FilePath ((</>), takeDirectory, takeFileName)
import           System.Posix hiding (release)
import           System.IO.Error (isDoesNotExistError)
import           Data.Text (unpack)

-- local imports
import qualified Common as C
import           Streams

-- | File storage.
data FileStore = FileStore
    { filePath  :: FilePath
    } deriving (Generic, Eq, Show)

instance ToJSON FileStore where
    toJSON (FileStore s) = toJSON s

instance FromJSON FileStore where
    parseJSON = withText "FileStore" $ pure . FileStore . unpack

instance IsString FileStore where
    fromString = FileStore

fileStoreOptions :: Opt.Parser FileStore
fileStoreOptions = FileStore
    <$> Opt.strOption
        ( Opt.long "path"
       <> Opt.metavar "FILE"
       <> Opt.help "Filename, '-' for stdin/stdout"
        )

-- | File rotate options (for File output).
data Rotate = Rotate
    { rotateKeep :: Int
    , rotateSize :: Maybe Integer
    , rotateTime :: Maybe Double
    } deriving (Generic, Eq, Show)

instance ToJSON Rotate
instance FromJSON Rotate

rotateOptions :: Opt.Parser Rotate
rotateOptions = Rotate
    <$> Opt.option Opt.auto
        ( Opt.long "rotateKeep"
       <> Opt.help "Keep number of rotated files"
        )
    <*> Opt.optional (Opt.option C.kiloMega
        ( Opt.long "rotateSize"
       <> Opt.help "Rotate file after <SIZE>[kMG] bytes"
       <> Opt.metavar "SIZE"
        ))
    <*> Opt.optional (Opt.option humanTime
        ( Opt.long "rotateTime"
       <> Opt.help "Rotate file after <T>[s|min|h|day]"
       <> Opt.metavar "T"
        ))

-- | Convert some time units to seconds (eg. 1min -> 60.0).
humanTime :: Opt.ReadM Double
humanTime = Opt.eitherReader $ \arg -> do
    let suffix :: [(String,Double)]
        suffix =
            [ ("", 1)
            , ("s", 1)
            , ("min", 60)
            , ("h", 3600)
            , ("day", 24*3600)
            ]
        (a,b) = span (flip elem ['0'..'9']) arg
    factor <- case lookup b suffix of
        Nothing -> Left $ "cannot parse suffix `" ++ b ++ "'"
        Just val -> Right val
    case reads a of
        [(r, "")] -> return (r * factor)
        _         -> Left $ "cannot parse value `" ++ arg ++ "'"

now :: IO (Time.UTCTime, POSIXTime)
now = (,) <$> Time.getCurrentTime <*> Data.Time.Clock.POSIX.getPOSIXTime

class IsStream w where
    writeMsg   :: w -> BS.ByteString -> IO ()
    closeWriter :: w -> IO ()
    writerName :: w -> Maybe FilePath

data Stream

    -- Stream to stdout.
    = StreamStdout

    -- File writer (fast version).
    -- Open file at startup and keep it open (until rotation).
    -- Example usage: regular recording to a file.
    | StreamHandle FilePath IO.Handle

    -- File writer (slow version).
    -- Apend and close on each write.
    -- It does not open a file until is required.
    -- Example usage: dump overflow data to a file.
    | StreamPath FilePath

instance IsStream Stream where
    writeMsg = \case
        StreamStdout -> BS.putStr
        StreamHandle _ h -> BS.hPut h
        StreamPath fp -> BS.appendFile fp
    closeWriter = \case
        StreamStdout -> return ()
        StreamHandle _ h -> IO.hClose h
        StreamPath _fp -> return ()
    writerName = \case
        StreamStdout -> Nothing
        StreamHandle fp _ -> Just fp
        StreamPath fp -> Just fp

-- | Create stdout stream.
streamStdout :: IO (Stream, Integer, POSIXTime)
streamStdout = (,,) <$> pure StreamStdout <*> pure 0 <*> fmap snd now

-- | Create fast file stream.
streamHandle :: FilePath -> IO (Stream, Integer, POSIXTime)
streamHandle fp = do
    -- get file status and make sure it exists
    st <- tryJust (guard . isDoesNotExistError) (getFileStatus fp) >>= \case
        Right st -> return st
        Left _ -> do
            IO.openFile fp IO.AppendMode >>= IO.hClose
            getFileStatus fp
    h <- IO.openFile fp IO.AppendMode
    return (StreamHandle fp h, fromIntegral $ fileSize st, accessTimeHiRes st)

-- | Create file stream.
streamPath :: FilePath -> IO (Stream, Integer, POSIXTime)
streamPath fp = do
    let fa = StreamPath fp
    tryJust (guard . isDoesNotExistError) (getFileStatus fp) >>= \case
        Right st -> return (fa, fromIntegral $ fileSize st, accessTimeHiRes st)
        Left _ -> (,,) <$> pure fa <*> pure 0 <*> fmap snd now

-- | File writer with 'rotation' support.
rotatingFileWriter :: (IsStream w) => IO (w, Integer, POSIXTime)
    -> Maybe Rotate -> (String -> IO ()) -> Consumer BS.ByteString ()
rotatingFileWriter openWriter mRot onRotate = writeSome --> writeSome
  where
    -- Write to one file. When it's time to rotate, terminate.
    writeSome = mkConsumer $ \consume -> do
        bracket rotate release (loop consume)

    rotateTo (nowUtc, nowPosix) (fw, bytes, t) = do
        let tFormat = Time.formatTime Time.defaultTimeLocale
                (Time.iso8601DateFormat (Just "%H-%M-%S"))
        rot <- mRot
        fs <- writerName fw
        let fileAge = nowPosix - t
            sizeLimit = maybe False (bytes >=) (rotateSize rot)
            timeLimit = case rotateTime rot of
                Nothing -> False
                Just x -> (toRational fileAge) >= (toRational x)
        guard $ sizeLimit || timeLimit
        Just (fs, fs ++ "-" ++ tFormat nowUtc, rotateKeep rot)

    loop consume (fw, !bytes, t) = atomically consume >>= \case
        EndOfData rv -> return rv
        Message msg -> do
            (nowUtc, nowPosix) <- now
            writeMsg fw msg
            let bytes' = bytes + (fromIntegral $ BS.length msg)
            case rotateTo (nowUtc, nowPosix) (fw, bytes', t) of
                Nothing -> loop consume (fw, bytes', t)
                Just _ -> return ()

    release (fw,_,_) = do
        closeWriter fw

    -- rotate files and open writer
    rotate = do
        (nowUtc, nowPosix) <- now
        (fw, bytes, t) <- openWriter
        case rotateTo (nowUtc, nowPosix) (fw, bytes, t) of
            Nothing -> return (fw, bytes, t)
            Just (fs, newName, keep) -> do
                (fw', b', t') <- mask_ $ do
                    closeWriter fw
                    renameFile fs newName
                    openWriter
                removed <- cleanup fs keep
                onRotate $ "Output file rotated: " ++ newName
                forM_ removed $ \i -> do
                    onRotate $ "Old file removed: " ++ i
                return (fw', b', t')

    cleanup fs keep = do
        let dirName = takeDirectory fs
        cont <- listDirectory dirName
        let olds = drop keep . reverse . sort . Prelude.filter myFiles $ cont
        mapM_ removeFile (fmap (dirName </>) olds)
        return olds
      where
        myFiles = isPrefixOf $ (takeFileName fs) ++ "-"

-- | Read chunks from file.
fileReaderChunks :: Int -> FileStore -> Producer BS.ByteString ()
fileReaderChunks chunkSize fs = mkProducer $ \produce -> do
    bracket acquire release (action produce)
  where
    acquire = case filePath fs of
        "-" -> return IO.stdin
        f -> IO.openFile f IO.ReadMode
    release h = case filePath fs of
        "-" -> return ()
        _ -> IO.hClose h
    action produce h = fix $ \loop -> do
        val <- BS.hGetSome h chunkSize
        case BS.null val of
            True -> return ()
            False -> do
                _ <- atomically $ produce val
                loop

-- | Read lines from file.
fileReaderLines :: FileStore -> Producer BS.ByteString ()
fileReaderLines fs = mkProducer $ \produce -> do
    bracket acquire release (action produce)
  where
    acquire = do
        h <- case filePath fs of
            "-" -> return IO.stdin
            f -> IO.openFile f IO.ReadMode
        IO.hSetBuffering h IO.LineBuffering
        return h
    release h = case filePath fs of
        "-" -> return ()
        _ -> IO.hClose h
    action produce h = fix $ \loop -> do
        rv <- try $ BS.hGetLine h
        case rv of
            Left (_e::SomeException) -> return ()
            Right msg -> do
                _ <- atomically $ produce msg
                loop
