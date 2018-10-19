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
import qualified Data.Time.Clock.POSIX
import           System.Directory (renameFile, listDirectory, removeFile)
import           System.FilePath ((</>), takeDirectory, takeFileName)
import           System.Posix (getFileStatus, fileSize, accessTimeHiRes)
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

-- | File writer with 'rotation' support.
fileWriter :: FileStore -> Maybe Rotate -> (String -> IO ())
    -> Consumer BS.ByteString c
fileWriter (FileStore "-") _ _ = consumeToIO BS.putStr
fileWriter (FileStore fs) mRot onRotate = mkConsumer $ \consume -> do
    -- get file status and make sure it exists
    (h, bytes, t) <- do
        st <- tryJust (guard . isDoesNotExistError) (getFileStatus fs) >>= \case
            Right st -> return st
            Left _ -> do
                IO.openFile fs IO.AppendMode >>= IO.hClose
                getFileStatus fs
        h <- IO.openFile fs IO.AppendMode
        return (h, fromIntegral $ fileSize st, accessTimeHiRes st)
    loop consume h t bytes
  where
    myFiles = isPrefixOf $ (takeFileName fs) ++ "-"
    now = (,) <$> Time.getCurrentTime <*> Data.Time.Clock.POSIX.getPOSIXTime
    tFormat = Time.formatTime Time.defaultTimeLocale
        (Time.iso8601DateFormat (Just "%H-%M-%S"))

    loop consume h t !bytes = atomically consume >>= \case
        EndOfData rv -> do
            IO.hClose h
            return rv
        Message msg -> do
            (nowUtc, nowPosix) <- now
            BS.hPut h msg
            let bytes' = bytes + (fromIntegral $ BS.length msg)
                rotateTo = do
                    rot <- mRot
                    let fileAge = nowPosix - t
                        sizeLimit = maybe False (bytes' >=) (rotateSize rot)
                        timeLimit = case rotateTime rot of
                            Nothing -> False
                            Just x -> (toRational fileAge) >= (toRational x)
                    guard $ sizeLimit || timeLimit
                    Just (fs ++ "-" ++ tFormat nowUtc, rotateKeep rot)
            case rotateTo of
                Nothing -> loop consume h t bytes'
                Just (newName, keep) -> do
                    h' <- mask_ $ do
                        IO.hClose h
                        renameFile fs newName
                        IO.openFile fs IO.AppendMode
                    removed <- cleanup keep
                    onRotate $ "Output file rotated: " ++ newName
                    forM_ removed $ \i -> do
                        onRotate $ "Old file removed: " ++ i
                    loop consume h' nowPosix 0

    cleanup keep = do
        let dirName = takeDirectory fs
        content <- listDirectory dirName
        let oldFiles =
                drop keep . reverse . sort . Prelude.filter myFiles $ content
        mapM_ removeFile (fmap (dirName </>) oldFiles)
        return oldFiles

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

