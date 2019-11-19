
module Time where

import           Data.Time
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Time.Clock.POSIX as DTP
import qualified System.Clock
import           Numeric (showFFloat)

type MonoTime = Integer
type UtcTime = UTCTime

-- | Get current monotonic time in nanoseconds.
getMonoTimeNs :: IO MonoTime
getMonoTimeNs = System.Clock.toNanoSecs <$> System.Clock.getTime System.Clock.Boottime

-- | Get current UTC time.
getUtcTime :: IO UtcTime
getUtcTime = getCurrentTime

-- | Get POSIX time.
getPOSIXTime :: IO DTP.POSIXTime
getPOSIXTime = DTP.getPOSIXTime

-- | Convert nanosecs time difference to days.
uptimeDaysStr :: MonoTime -> MonoTime -> String
uptimeDaysStr t0 t =
    let uptime = fromIntegral $ t - t0
        uptimeSec = uptime / (10^(9::Int))
        uptimeDays = (uptimeSec :: Double) / (24*3600)
    in showFFloat (Just 3) uptimeDays "" ++ " days"

-- | Parse ISO time.
parseIsoTime :: String -> Either String UtcTime
parseIsoTime s = case iso8601ParseM s of
    Nothing -> Left $ "Can not parse time: " ++ show s
    Just val -> Right val

