------------------
-- |
-- Module: File
--
-- This module provides common File definitions.
--

module File
( FileStore(..)
, fileStoreOptions
, File.appendFile
, rotateFile
) where

import qualified Data.ByteString as BS
import Data.List (isPrefixOf, sort)
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX
import Data.Monoid ((<>))
import qualified Options.Applicative as Opt
import qualified System.IO
import System.Directory (renameFile, listDirectory, removeFile)
import System.FilePath ((</>), takeDirectory)
import System.Posix (getFileStatus, fileSize, accessTimeHiRes)

-- local imports
import qualified Encodings as Enc

-- | File storage.
data FileStore = FileStore
    { filePath  :: FilePath
    , fileEnc   :: Enc.EncodeFormat
    } deriving (Eq, Show)

fileStoreOptions :: Opt.Parser FileStore
fileStoreOptions = FileStore
    <$> Opt.strOption
        ( Opt.long "path"
       <> Opt.metavar "FILE"
       <> Opt.help "Filename, '-' for stdin/stdout"
        )
    <*> Enc.encodeFormatOptions

-- | Append some data to a file.
appendFile :: (Enc.Encodable msg) => FileStore -> [msg] -> IO ()
appendFile (FileStore path fmt) messages = do
    let s = Enc.encodeList fmt messages
    case path of
        "-" -> BS.hPut System.IO.stdout s
        _ -> BS.appendFile path s

-- | Rotate a file if limit reached.
rotateFile :: FileStore -> Int -> Maybe Integer -> Maybe Double
    -> IO (Maybe (FilePath, [FilePath]))
rotateFile (FileStore "-" _) _ _ _ = return Nothing
rotateFile (FileStore path _) keep mSize mTime = do
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

