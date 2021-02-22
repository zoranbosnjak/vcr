
module Replay
    ( module Replay.Types
    , module Common
    , module Udp
    , module Time
    , module Vcr
    , module Streaming
    , runReplay

    , module Control.Monad
    , module Options.Applicative
    , module Pipes
    ) where

import           UnliftIO
import           Control.Monad
import           Options.Applicative
import           Pipes

-- local import
import           Common
import           Udp
import           Replay.Types
import           Streaming
import           Vcr
import           Time
import           Replay.Controller (controller)
import           Replay.View.Wx    (runUI)          -- use WX user interface

-- | Tick period in miliseconds.
tickMs :: Int
tickMs = 100

-- | Choices for speed selector
speedChoices :: [Double]
speedChoices = [10 ** (x/10) | x <- [-10..10]]

-- | Run replay GUI.
runReplay ::
    Int                     -- console buffer size
    -> [(Name, Source)]     -- event sources
    -> [(Name, Channel -> Channel)] -- channel maps
    -> [(Name, [(Channel, BlinkTime, ConsoleDump, Output)])]   -- outputs
    -> IO ()
runReplay maxDump sources channelMaps outputs = do
    tUtc <- getUtcTime >>= newTVarIO
    runUI tUtc maxDump speedChoices sources channelMaps outputs
        (controller tUtc) tickMs

