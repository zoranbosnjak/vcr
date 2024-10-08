-- | Monotonic and UTC time routines.

module Time
    ( module Time
    , module Data.Time
    , module Data.Time.Clock
    )
where

import           Data.Time
import           Data.Time.Clock
import qualified Data.Time.Clock.POSIX    as DTP
import           Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import           Numeric                  (showFFloat)
import qualified System.Clock

import           Test.QuickCheck

type MonoTimeNs = Integer
type UtcTime = UTCTime

-- | Get current monotonic time in nanoseconds.
getMonoTimeNs :: IO MonoTimeNs
getMonoTimeNs = System.Clock.toNanoSecs <$> System.Clock.getTime System.Clock.Boottime

-- | Get current UTC time.
getUtcTime :: IO UtcTime
getUtcTime = getCurrentTime

-- | Get POSIX time.
getPOSIXTime :: IO DTP.POSIXTime
getPOSIXTime = DTP.getPOSIXTime

-- | Convert nanosecs time difference to days.
uptimeDaysStr :: MonoTimeNs -> MonoTimeNs -> String
uptimeDaysStr t0 t =
    let uptime = fromIntegral $ t - t0
        uptimeSec = uptime / (10^(9::Int))
        uptimeDays = (uptimeSec :: Double) / (24*3600)
    in showFFloat (Just 3) uptimeDays "" ++ " days"

-- | Parse ISO time.
parseIsoTime :: String -> Either String UtcTime
parseIsoTime s = case iso8601ParseM s of
    Nothing  -> Left $ "Can not parse time: " ++ show s
    Just val -> Right val

-- | Format ISO time.
fmtTime :: UtcTime -> String
fmtTime = iso8601Show

-- | Helper function to generate arbitrary Utc time.
arbitraryUtc :: Gen UtcTime
arbitraryUtc = UTCTime
    <$> (fromGregorian <$> fmap getPositive arbitrary <*> choose (1,12) <*> choose (1,31))
    <*> (picosecondsToDiffTime <$> choose (0, 24*3600*1000*1000*1000*1000-1))

-- | Helper function to increment Utc time:
addMonoTimeNS :: MonoTimeNs -> UtcTime -> UtcTime
addMonoTimeNS deltaNs = addUTCTime
    (fromRational (toRational deltaNs / (1000*1000*1000)))
