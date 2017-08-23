------------------
-- |
-- Module: File
--
-- This module provides common File definitions.
--

{-# LANGUAGE ScopedTypeVariables #-}

module File where

import           Control.Exception (try, SomeException)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import           Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified System.IO as IO
import qualified Data.Time as Time
import           Data.List (isPrefixOf, sort)
import qualified Data.Time.Clock.POSIX
import           System.Directory (renameFile, listDirectory, removeFile)
import           System.FilePath ((</>), takeDirectory)
import           System.Posix (getFileStatus, fileSize, accessTimeHiRes)

-- local imports
import qualified Common as C
import           Streams hiding (filter)

-- | File storage.
data FileStore = FileStore
    { filePath  :: FilePath
    } deriving (Eq, Show)

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
    } deriving (Eq, Show)

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
fileWriter :: FileStore -> Maybe Rotate -> Pipe BS.ByteString String
fileWriter fs mRotate = mkPipe action
  where
    action consume produce = forever $ do
        consume >>= liftIO . appendToFile fs
        rotateResult <- maybe
            (return Nothing)
            (\rot -> liftIO $ rotateFile fs rot)
            mRotate
        case rotateResult of
            Nothing -> return ()
            Just (new, old) -> do
                _ <- produce $ "Output file rotated: " ++ new
                forM_ old $ \i -> do
                    produce $ "Old file removed: " ++ i

    appendToFile (FileStore "-") = BS.putStr
    appendToFile (FileStore f) = BS.appendFile f

    -- | Rotate a file if limit reached.
    rotateFile :: FileStore -> Rotate -> IO (Maybe (FilePath, [FilePath]))
    rotateFile (FileStore "-") _rotate = return Nothing
    rotateFile (FileStore path) (Rotate keep mSize mTime) = do
        nowUtc <- Time.getCurrentTime
        nowPosix <- Data.Time.Clock.POSIX.getPOSIXTime
        st <- getFileStatus path

        let fileAge = nowPosix - accessTimeHiRes st

            sizeLimit = maybe False ((fromIntegral . fileSize) st >=) mSize
            timeLimit = case mTime of
                Nothing -> False
                Just time -> (toRational fileAge) >= (toRational time)

        case (sizeLimit || timeLimit) of
            False -> return Nothing
            True -> do
                let newName = path ++ "-" ++ tFormat nowUtc
                renameFile path newName
                removed <- cleanup
                return $ Just (newName, removed)

      where

        tFormat = Time.formatTime
            Time.defaultTimeLocale
            (Time.iso8601DateFormat (Just "%H-%M-%S"))

        cleanup = do
            let dirName = takeDirectory path
            content <- fmap (dirName </>) <$> listDirectory dirName
            let oldFiles = drop keep . reverse . sort . filter myFiles $ content
            mapM_ removeFile oldFiles
            return oldFiles

        myFiles = isPrefixOf path

-- | Read chunks from file.
fileReaderChunks :: Int -> FileStore -> Producer BS.ByteString
fileReaderChunks chunkSize fs = mkProducer action where
    acquire = case filePath fs of
        "-" -> return IO.stdin
        f -> liftIO $ IO.openFile f IO.ReadMode
    release h = case filePath fs of
        "-" -> return ()
        _ -> liftIO $ IO.hClose h
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
            f -> liftIO $ IO.openFile f IO.ReadMode
        liftIO $ IO.hSetBuffering h IO.LineBuffering
        return h
    release h = case filePath fs of
        "-" -> return ()
        _ -> liftIO $ IO.hClose h
    action produce = bracket acquire release $ \h -> do
        let loop = do
                rv <- liftIO $ try $ BS.hGetLine h
                case rv of
                    Left (_e::SomeException) -> return ()
                    Right val -> do
                        _ <- produce val
                        loop
        loop

