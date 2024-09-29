-- | Common replay data type definitions.

module Replay.Types where

import           Data.ReactiveValue
import           Pipes
import qualified Pipes.Safe         as PS
import           UnliftIO

-- local imports
import           Streaming
import           Time
import           Udp                (UdpContent)
import           Vcr

type Name = String
type Tooltip = String

-- | Output attributes.
type BlinkTime = Event UdpContent -> Maybe Double
type ConsoleDump = Event UdpContent -> String
data Output = Output
    { outConsumer :: Consumer (Event UdpContent) (PS.SafeT IO) ()
    , outTooltip  :: Tooltip
    }

-- | Console actions
data ConsoleAction
    = ConsoleClear
    | ConsolePrint String

-- | What to do when replay reaches the marker.
data AtMarker = Continue | Wrap | Stop
    deriving (Eq, Enum, Bounded, Show)

data UISource = UISourceRecorder | UISourceFile
    deriving (Eq, Show, Bounded, Enum)

-- | User interface API.
data UI = UI
    { uiExit        :: IO ()

    , uiSource      :: ReactiveFieldRead IO UISource
    , uiRecorder    :: ReactiveFieldRead IO Source
    , uiFile        :: ReactiveFieldRead IO (Maybe FilePath)

    , uiChannelMap  :: ReactiveFieldRead IO (Channel -> Channel)

    , uiLimits      :: Maybe (UtcTime, UtcTime) -> STM ()

    , uiConsole     :: ConsoleAction -> STM ()

    , uiBufferLevel :: Int -> STM ()

    , uiTSet        :: ReactiveFieldRead IO UtcTime

    , uiMarker1     :: ReactiveFieldRead IO UtcTime
    , uiMarker2     :: ReactiveFieldRead IO UtcTime

    , uiDirection   :: ReactiveFieldRead IO Direction
    , uiAtMarker    :: ReactiveFieldRead IO AtMarker
    , uiSpeed       :: ReactiveFieldRead IO Double
    , uiRunning     :: ReactiveFieldRead IO Bool

    , uiOutputSet   :: ReactiveFieldRead IO Name
    , uiOutputSelection ::
        [ (Name, [ ( Channel
                   , ( ReactiveFieldRead IO (Maybe ConsoleDump)
                     , ReactiveFieldRead IO (Maybe (Consumer (Event UdpContent) (PS.SafeT IO) ()))
                     , Event UdpContent -> IO () ) ) ]) ]
    }

