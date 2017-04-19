------------------
-- |
-- Module: File
--
-- This module provides common File definitions.
--

module File
( FileStore
, fileStoreOptions
, File.appendFile
) where

import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Options.Applicative as Opt
import qualified System.IO

-- local imports
import qualified Encodings as Enc

-- | File storage.
data FileStore = FileStore FilePath Enc.EncodeFormat deriving (Eq, Show)

fileStoreOptions :: Opt.Parser FileStore
fileStoreOptions = FileStore
    <$> Opt.strOption
        ( Opt.long "path"
       <> Opt.metavar "FILE"
       <> Opt.help "Filename, '-' for stdin/stdout"
        )
    <*> Enc.encodeFormatOptions

appendFile :: (Enc.Encodable msg) => FileStore -> [msg] -> IO ()
appendFile (FileStore path fmt) messages = do
    let s = Enc.encodeList fmt messages
    case path of
        "-" -> BS.hPut System.IO.stdout s
        _ -> BS.appendFile path s

