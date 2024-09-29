{-# LANGUAGE DeriveAnyClass #-}

-- | Common type definitions.

module Capture.Types where

import           Data.Aeson     (FromJSON, ToJSON)
import qualified Data.Map       as Map
import           GHC.Generics   (Generic)

-- local imports
import           Streaming.Disk (Rotate)
import           Udp            (UdpIn)
import           Vcr            (Channel)

data Config = Config
    { confInputs     :: Map.Map Channel UdpIn
    , confOutputFile :: Maybe (FilePath, Rotate)
    } deriving (Generic, Eq, Show, ToJSON, FromJSON)

emptyConfig :: Config
emptyConfig = Config
    { confInputs = mempty
    , confOutputFile = Nothing
    }

