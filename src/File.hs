------------------
-- |
-- Module: File
--
-- This module provides common File definitions.
--

module File
( FileStore
, fileStoreOptions
) where

import Data.Monoid ((<>))
import qualified Options.Applicative as Opt

-- local imports
import qualified Encodings as Enc

-- | File storage.
data FileStore = FileStore FilePath Enc.EncodeFormat deriving (Eq, Show)

fileStoreOptions :: Opt.Parser FileStore
fileStoreOptions = FileStore
    <$> Opt.strOption
        ( Opt.long "path"
       <> Opt.metavar "FILE"
       <> Opt.help "Filename"
        )
    <*> Enc.encodeFormatOptions

