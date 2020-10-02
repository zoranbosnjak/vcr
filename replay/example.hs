{- |
Description : Replay configuration example.

This module is an example replay configuration.

Usage:

# to run a program.
vcr custom --program "</abs/path/to/this/script> --asterix <abs/path/to/asterix/files>" --run

# to check
vcr custom --program "</abs/path/to/this/script>" --validate

# to make it executable, use shebang, something like this
#! /usr/bin/env -S vcr-custom.sh --asterix /path/to/asterix-data/xml
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- standard imports
import           Options.Applicative
import           Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString as BS
import           Data.Time
import           Data.List (nub)
import           Data.Bool
import           Data.Char (toUpper)
import           Data.Word
import           Text.Printf
import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>))
import           Pipes
import qualified Pipes.Safe as PS
import qualified Pipes.Prelude as PP

-- VCR imports
import           Common (hexlify)
import           Replay
import qualified Udp
import           Time
import           Vcr

-- Asterix processor
import qualified Data.Asterix as Ast
import qualified Data.BitString as Bits

-- List of recorders.
recorders :: [(Name, Recorder)]
recorders =
    [ ("rec1", "http://127.0.0.1:12345")
    , ("rec2", "https://127.0.0.1:12346")
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
        _ -> ch <> "Test"           -- append "Test" to other channels,
                                    -- such that "ch1" becomes "ch1Test"
      )
    -- add more as necessary
    ]

-- | This is an example dump function.
-- In this example, show utc time, channel name and
-- the first part of the datagram.
dump :: PrintfType t => Event Udp.UdpContent -> t
dump evt = printf "%s: %-10s: 0x%-10s...\n"
    (tf $ eTimeUtc evt)
    (T.take 10 $ eChannel evt)
    (fmap toUpper $ take 10 $ hexlify $ Udp.udpDatagram $ eValue evt)
  where
    tf = formatTime defaultTimeLocale "%H:%M:%S%3Q"

-- | Replay filter example - prepend byte to datagram.
prepend :: MonadIO m => Word8 -> Pipe BS.ByteString BS.ByteString m ()
prepend b = PP.map (\bs -> BS.singleton b <> bs)

-- | Replay filter example.
-- It manipulates timestamp in some asterix records,
-- such that it looks like the record was just generated.
-- Take into consideration original delay when restamping.
dgRestamp :: MonadIO m => Ast.Profiles -> Pipe (Event Udp.UdpContent) BS.ByteString m ()
dgRestamp uaps = forever $ do
    event <- await
    now <- tod <$> liftIO getUtcTime
    yield $ processDatagram
        now
        (tod $ eTimeUtc event)
        (Udp.udpDatagram $ eValue event)
  where

    tod :: UtcTime -> Ast.EValue
    tod t = Ast.EDouble $ fromRational $ toRational $ utctDayTime t

    processDatagram :: Ast.EValue -> Ast.EValue -> BS.ByteString -> BS.ByteString
    processDatagram now oldUtc s = case Ast.toDataBlocks $ Bits.fromByteString s of
        Nothing -> s    -- not an asterix
        Just dbs ->     -- process each datablock, encode, check alignment
            let result = mconcat $ fmap (Ast.fromDataBlock . processDataBlock) dbs
            in bool s (Bits.toByteString result) (Bits.isAligned result)
      where
        processDataBlock :: Ast.DataBlock -> Ast.DataBlock
        processDataBlock db = case mProcessRecord of
            Nothing -> db
            Just processRecord -> case Ast.toRecords uaps db of
                Nothing -> db
                Just recs -> Ast.mkDataBlock (Ast.dbCat db) (fmap processRecord recs)
          where
            mProcessRecord :: Maybe (Ast.Item -> Ast.Item)
            mProcessRecord = fmap modifyItem itemToModify
              where
                itemToModify = case Ast.dbCat db of
                    1  -> Just "141"
                    2  -> Just "030"
                    19 -> Just "140"
                    20 -> Just "140"
                    34 -> Just "030"
                    48 -> Just "140"
                    _ -> Nothing
                modifyItem i rec = maybe rec id $ Ast.update rec $ do
                    Ast.modifyItem i $ \j dsc -> do
                        t1 <- Ast.toRaw j
                        t2 <- Ast.fromNatural oldUtc dsc >>= Ast.toRaw
                        t3 <- Ast.fromNatural now dsc >>= Ast.toRaw
                        let originalDelay = t2 - t1
                            t4 = t3 - originalDelay
                        Ast.fromRaw (t4 :: Int) dsc

-- | Extract datagram from event.
dg :: Monad m => Pipe (Event Udp.UdpContent) BS.ByteString m ()
dg = PP.map (Udp.udpDatagram . eValue)

-- Send event to UDP unicast.
txUnicast :: Udp.Ip -> Udp.Port -> Consumer BS.ByteString (PS.SafeT IO) ()
txUnicast ip port = Udp.udpWriter (Udp.UdpOutUnicast ip port)

-- Send event to UDP multicast.
txMulticast :: Udp.Ip -> Udp.Port -> Udp.Ip -> Udp.TTL
    -> Consumer BS.ByteString (PS.SafeT IO) ()
txMulticast ip port localIp ttl =
    Udp.udpWriter (Udp.UdpOutMulticast ip port (Just localIp) (Just ttl))

blink :: Double -> Udp.UdpEvent -> Maybe Double
blink t _event = Just t

noBlink :: Udp.UdpEvent -> Maybe Double
noBlink _event = Nothing

-- | Replay sessions.
outputs ::
    Ast.Profiles        -- asterix profiles
    -> [ (Name      -- session name
       , [ ( Channel                                -- channel name
           , Udp.UdpEvent -> Maybe Double           -- seconds to switch on active indicator
           , Udp.UdpEvent -> String                 -- console dump function
           , Consumer Udp.UdpEvent (PS.SafeT IO) () -- event consumer
           , String                                 -- tooltip string
           )
         ])
       ]
outputs uaps =
    [ ("normal",   -- normal replay
        [ ("ch1", blink 1.0, dump, dg >-> txUnicast "127.0.0.1" "59001", "local 59001")
        , ("ch2", blink 0.1, dump, dg >-> txUnicast "127.0.0.1" "59002", "local 59002")
        , ("ch3", blink 0.2, dump, dg >-> txUnicast "127.0.0.1" "59003", "local 59003")
        , ("ch4", noBlink,   dump, dg >-> txUnicast "127.0.0.1" "59004", "local 59004")
        ])
    , ("prepend",   -- prepend byte
        [ ("ch1", blink 1.0, dump, dg >-> prepend 1 >-> txUnicast "127.0.0.1" "59001", "local 59001")
        , ("ch2", blink 1.0, dump, dg >-> prepend 2 >-> txUnicast "127.0.0.1" "59002", "local 59002")
        , ("ch3", blink 1.0, dump, dg >-> prepend 3 >-> txUnicast "127.0.0.1" "59003", "local 59003")
        , ("ch4", blink 1.0, dump, dg >-> prepend 4 >-> txUnicast "127.0.0.1" "59004", "local 59004")
        ])
    , ("restamp",   -- restamp asterix
        [ ("ch1", blink 1.0, dump, dgRestamp uaps >-> txUnicast "127.0.0.1" "59001", "local 59001")
        , ("ch2", blink 1.0, dump, dgRestamp uaps >-> txUnicast "127.0.0.1" "59002", "local 59002")
        , ("ch3", blink 1.0, dump, dgRestamp uaps >-> txUnicast "127.0.0.1" "59003", "local 59003")
        , ("ch4", blink 1.0, dump, dgRestamp uaps >-> txUnicast "127.0.0.1" "59004", "local 59004")
        ])
    , ("multicast",   -- send to multicast
        [ ("ch1", blink 1.0, dump, dg >-> txMulticast "239.0.0.1" "59001" "127.0.0.1" 32, "mc 59001")
        , ("ch2", blink 1.0, dump, dg >-> txMulticast "239.0.0.1" "59002" "127.0.0.1" 32, "mc 59002")
        , ("ch3", blink 1.0, dump, dg >-> txMulticast "239.0.0.1" "59003" "127.0.0.1" 32, "mc 59003")
        , ("ch4", blink 1.0, dump, dg >-> txMulticast "239.0.0.1" "59004" "127.0.0.1" 32, "mc 59004")
        ])
    ]

data Options = Options
    { optAstData    :: [FilePath]
    } deriving (Show)

options :: Parser Options
options = Options
    <$> some (strOption (long "asterix" <> help "Asterix data definition directory"))

-- A main program, start replay GUI.
main :: IO ()
main = do
    opt <- execParser (info (options <**> helper) idm)

    -- read asterix definition files
    uaps <- do
        let paths = optAstData opt
        files <- collectFiles [".xml", ".XML"] paths
        when (null files) $
            error $ "no xml files found in " ++ show paths
        descriptions <- forM files $ \filename -> do
            xml <- readFile filename
            case Ast.categoryDescription xml of
                Left msg -> error $ filename ++ ", " ++ msg
                Right val -> return val
        -- get latest revisions
        case Ast.categorySelect descriptions [] of
                Left msg -> error msg
                Right val -> return val

    -- run gui
    replayGUI
        (50*1000) -- console buffer size
        recorders channelMaps (outputs uaps)

-- | Get specific files from list of directories.
collectFiles :: [String] -> [FilePath] -> IO [FilePath]
collectFiles extensions dirs = nub . filter match . concat <$> mapM ls dirs
  where
    ls dir = do
        listing <- filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
        return $ map (\x -> dir </> x) listing
    match filename = any (matchSuffix filename) extensions
    matchSuffix filename suffix
        | length filename < length suffix = False
        | otherwise = and $ zipWith (==) (reverse filename) (reverse suffix)

