-- | Description : Replay configuration example.

{-# LANGUAGE OverloadedStrings #-}

-- standard imports
import qualified Data.ByteString as BS
import           Data.Char       (toUpper)
import qualified Data.Text       as T
import           Data.Word
import qualified Pipes.Prelude   as PP
import qualified Pipes.Safe      as PS
import           Text.Printf

-- VCR imports
import           Replay

-- List of recorders.
recorders :: [(Name, Source)]
recorders =
    [ ("dir1", SDirectory TextEncoding "rec/recording")
    , ("rec1", SHttp "http://127.0.0.1:12345/")
    , ("rec2", SHttp "https://127.0.0.1:12346/")
    -- add more recorders as necessary
    ]

-- | Channel mapping.
-- This might be necessary to handle channel naming from different recorders,
-- just for flexibility reasons.
-- For example:
--  - test recorder might add "Test" suffix to each channel name.
--  - In order to get channel "ch1" from the normal recorder,
--    name remains the same, however if the "ch1" is requested from a
--    test recorder, a "Test" suffix is necessary.
--  - In this case, when the recorder is changed, the mapping must
--    be changed too, but the output channel configuration remains the same.
channelMaps :: [(Name, Channel -> Channel)]
channelMaps =
    [ ("transparent", id)   -- no manipulation
    , ("chTest", \ch -> case ch of  -- append "Test"
        "special" -> ch             -- this name is exception, do not change
        _         -> ch <> "Test"           -- append "Test" to other channels,
                                    -- such that "ch1" becomes "ch1Test"
      )
    -- add more as necessary
    ]

-- | This is an example dump function.
-- In this example, show utc time, channel name and
-- the first part of the datagram.
dump :: PrintfType t => Event UdpContent -> t
dump evt = printf "%s: %-10s: 0x%-10s...\n"
    (tf $ eTimeUtc evt)
    (T.take 10 $ eChannel evt)
    (fmap toUpper $ take 10 $ hexlify $ udpDatagram $ eValue evt)
  where
    tf = formatTime defaultTimeLocale "%H:%M:%S%3Q"

-- | Replay filter example - prepend byte to datagram.
prepend :: MonadIO m => Word8 -> Pipe BS.ByteString BS.ByteString m ()
prepend b = PP.map (\bs -> BS.singleton b <> bs)

-- | Extract datagram from event.
dg :: Monad m => Pipe (Event UdpContent) BS.ByteString m ()
dg = PP.map (udpDatagram . eValue)

-- Send event to UDP unicast.
txUnicast ::
    Pipe (Event UdpContent) BS.ByteString (PS.SafeT IO) ()
    -> Ip -> Port -> Output
txUnicast prefilter ip port = Output
    (prefilter >-> udpWriter (UdpOutUnicast ip port))
    ("unicast "++ show ip ++ " " ++ show port)

-- Send event to UDP multicast.
txMulticast ::
    Pipe (Event UdpContent) BS.ByteString (PS.SafeT IO) ()
    -> Ip -> Port -> Ip -> TTL -> Output
txMulticast prefilter ip port localIp ttl = Output
    (prefilter >-> udpWriter (UdpOutMulticast ip port (Just localIp) (Just ttl)))
    ("multicast "++ show ip ++ " " ++ show port)

blink :: Double -> Event UdpContent -> Maybe Double
blink t _event = Just t

noBlink :: Event UdpContent -> Maybe Double
noBlink _event = Nothing

-- | Replay sessions.
outputs :: [ (Name      -- session name
    , [ ( Channel        -- channel name
        , BlinkTime      -- seconds to switch on active indicator
        , ConsoleDump    -- console dump function
        , Output         -- event consumer
        )]
    )]
outputs =
    [ ("normal",   -- normal replay
        [ ("ch1", blink 1.0, dump, txUnicast dg "127.0.0.1" "59001")
        , ("ch2", blink 0.1, dump, txUnicast dg "127.0.0.1" "59002")
        , ("ch3", blink 0.2, dump, txUnicast dg "127.0.0.1" "59003")
        , ("ch4", noBlink  , dump, txUnicast dg "127.0.0.1" "59004")
        , ("čšžtest", blink 1.0, dump, txUnicast dg "127.0.0.1" "59005")
        , ("ch6", blink 1.0, dump, txUnicast dg "127.0.0.1" "59006")
        , ("ch7", blink 1.0, dump, txUnicast dg "127.0.0.1" "59007")
        ])
    , ("prepend",   -- prepend byte
        [ ("ch1", blink 1.0, dump, txUnicast (dg >-> prepend 1) "127.0.0.1" "59001")
        , ("ch2", blink 1.0, dump, txUnicast (dg >-> prepend 2) "127.0.0.1" "59002")
        , ("ch3", blink 1.0, dump, txUnicast (dg >-> prepend 3) "127.0.0.1" "59003")
        , ("ch4", blink 1.0, dump, txUnicast (dg >-> prepend 4) "127.0.0.1" "59004")
        ])
    , ("multicast",   -- send to multicast
        [ ("ch1", blink 1.0, dump, txMulticast dg "239.0.0.1" "59001" "127.0.0.1" 32)
        , ("ch2", blink 1.0, dump, txMulticast dg "239.0.0.1" "59002" "127.0.0.1" 32)
        , ("ch3", blink 1.0, dump, txMulticast dg "239.0.0.1" "59003" "127.0.0.1" 32)
        , ("ch4", blink 1.0, dump, txMulticast dg "239.0.0.1" "59004" "127.0.0.1" 32)
        ])
    ]

-- A main program, start replay GUI.
main :: IO ()
main = do
    runReplay
        (50*1000) -- console buffer size
        recorders channelMaps outputs
