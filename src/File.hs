------------------
-- |
-- Module: File
--
-- This module provides common File definitions.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module File where

import           Control.Exception (try, tryJust, SomeException, mask_)
import           Control.Monad hiding (forever)
import           Control.Monad.IO.Class (liftIO)
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
import           Streams hiding (filter)

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
       <> Opt.help "Rotate file after <SEC>[s|min|h|day]"
       <> Opt.metavar "SEC"
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
fileWriter ::
    FileStore
    -> Maybe Rotate
    -> (String -> IO ())
    -> Consumer BS.ByteString
fileWriter (FileStore "-") _ _ = mkConsumer $ \consume -> forever $ do
    consume Clear >>= liftIO . BS.putStr
fileWriter (FileStore fs) mRot onRotate = mkConsumer action where

    now = (,) <$> Time.getCurrentTime <*> Data.Time.Clock.POSIX.getPOSIXTime

    acquire = do
        st <- do
            rv <- tryJust (guard . isDoesNotExistError) (getFileStatus fs)
            case rv of
                Left _ -> do
                    IO.openFile fs IO.AppendMode >>= IO.hClose
                    getFileStatus fs
                Right a -> return a
        hv <- IO.openFile fs IO.AppendMode >>= newTVarIO
        return (hv, fromIntegral $ fileSize st, accessTimeHiRes st)

    release (hv,_,_) = atomically (readTVar hv) >>= IO.hClose

    action consume = bracket acquire release loop where
        loop (hv,bytes,accessTime) = do
            h <- liftIO $ atomically $ readTVar hv
            msg <- consume Clear
            (nowUtc, nowPosix) <- liftIO now
            let rotArgs = do
                    rot <- mRot
                    let fileAge = nowPosix - accessTime
                        sizeLimit = maybe False (bytes >=) (rotateSize rot)
                        timeLimit = case rotateTime rot of
                            Nothing -> False
                            Just t -> (toRational fileAge) >= (toRational t)
                    guard $ sizeLimit || timeLimit
                    Just (fs ++ "-" ++ tFormat nowUtc, rotateKeep rot)
            (h',bytes',accessTime') <- case rotArgs of
                Nothing -> return (h,bytes,accessTime)
                Just (newName, keep) -> liftIO $ do
                    h' <- mask_ $ do
                        IO.hClose h
                        renameFile fs newName
                        h' <- IO.openFile fs IO.AppendMode
                        atomically $ writeTVar hv h'
                        return h'
                    removed <- cleanup keep
                    onRotate $ "Output file rotated: " ++ newName
                    forM_ removed $ \i -> do
                        onRotate $ "Old file removed: " ++ i
                    return (h', 0, nowPosix)
            liftIO $ BS.hPut h' msg
            loop (hv, bytes' + (fromIntegral $ BS.length msg), accessTime')

        tFormat = Time.formatTime
            Time.defaultTimeLocale
            (Time.iso8601DateFormat (Just "%H-%M-%S"))

        cleanup keep = do
            let dirName = takeDirectory fs
            content <- listDirectory dirName
            let oldFiles = drop keep . reverse . sort . filter myFiles $ content
            mapM_ removeFile (fmap (dirName </>) oldFiles)
            return oldFiles

        myFiles = isPrefixOf $ (takeFileName fs) ++ "-"

-- | Read chunks from file.
fileReaderChunks :: Int -> FileStore -> Producer BS.ByteString
fileReaderChunks chunkSize fs = mkProducer action where
    acquire = case filePath fs of
        "-" -> return IO.stdin
        f -> IO.openFile f IO.ReadMode
    release h = case filePath fs of
        "-" -> return ()
        _ -> IO.hClose h
    action produce = bracket acquire release $ \h -> do
        let loop = do
                val <- liftIO $ BS.hGetSome h chunkSize
                case BS.null val of
                    True -> return ()
                    False -> do
                        _ <- produce val
                        loop
        loop

-- | Read lines from file.
fileReaderLines :: FileStore -> Producer BS.ByteString
fileReaderLines fs = mkProducer action where
    acquire = do
        h <- case filePath fs of
            "-" -> return IO.stdin
            f -> IO.openFile f IO.ReadMode
        IO.hSetBuffering h IO.LineBuffering
        return h
    release h = case filePath fs of
        "-" -> return ()
        _ -> IO.hClose h
    action produce = bracket acquire release $ \h -> do
        let loop = do
                rv <- liftIO $ try $ BS.hGetLine h
                case rv of
                    Left (_e::SomeException) -> return ()
                    Right val -> do
                        _ <- produce val
                        loop
        loop

