{-# LANGUAGE DeriveAnyClass #-}

-- | Common type definitions.

module Capture.Types where

import           GHC.Generics (Generic)
import qualified Data.Map as Map
import           Data.Aeson (ToJSON, FromJSON)

-- local imports
import           Vcr (Channel)
import           Udp (UdpIn)
import           Streaming.Disk (Rotate)

data Config = Config
    { confInputs :: Map.Map Channel UdpIn
    , confOutputFile :: Maybe (FilePath, Rotate)
    } deriving (Generic, Eq, Show, ToJSON, FromJSON)

emptyConfig :: Config
emptyConfig = Config
    { confInputs = mempty
    , confOutputFile = Nothing
    }

