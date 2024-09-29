{-# LANGUAGE LambdaCase #-}

-- | This module reexports streaming components to/from
-- various sources (Disk, Http...).

module Streaming
    ( module Streaming
    , FileEncoding (..)
    )
where

import           Data.Aeson
import           GHC.Generics   (Generic)
import           Pipes.Safe

import qualified Streaming.Disk
import           Streaming.Disk (FileEncoding)
import qualified Streaming.Http
import           Vcr

data Source
    = SFile FileEncoding FilePath
    | SDirectory FileEncoding FilePath
    | SHttp String
    deriving (Generic, Eq, Show)

mkPlayer :: FromJSON a => Source -> Player (SafeT IO) Index (Event a)
mkPlayer = \case
    SFile enc path -> reindex $ Streaming.Disk.mkFilePlayer
        (Streaming.Disk.FileArchive enc path)
    SDirectory enc base -> reindex $ Streaming.Disk.mkDirectoryPlayer
        (Streaming.Disk.DirectoryArchive enc base)
    SHttp url -> reindex $ Streaming.Http.mkHttpPlayer
        (Streaming.Http.HttpArchive url)

