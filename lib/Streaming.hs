{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streaming
    ( module Streaming
    , FileEncoding (..)
    )
where

import           GHC.Generics (Generic)
import           Data.Aeson
import           Pipes.Safe

import           Vcr
import           Streaming.Disk (FileEncoding)
import qualified Streaming.Disk
import qualified Streaming.Http

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

