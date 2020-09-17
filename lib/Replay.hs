
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Replay where

import           GHC.Natural
import           Data.Maybe
import           Data.Bool
import           Data.Void
import           Data.Either (rights)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Exception (IOException)
import           Control.Monad.Trans.Except
import           Pipes
import qualified Pipes.Safe as PS
import           Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.Char (toLower)
import           Text.Printf
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Data.Time.Clock as Clk
import qualified Data.Time.Calendar as Cal

import           System.IO
import           System.Directory (doesPathExist)

import           Graphics.UI.WXCore
import           Graphics.UI.WX

import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception (try)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Client as HTC
import qualified Network.HTTP.Client.TLS as HTCS
import qualified Network.HTTP.Types as HT
import qualified Data.Aeson as AES

import           Data.String

-- local imports
import           Common
import           Time
import           Vcr
import           Udp
import           File

type Name = String
type Running = Bool
type Speed = Double

data Source = SrcRecorder | SrcFile
    deriving (Eq, Enum, Bounded, Show)

newtype Recorder = Recorder { unRecorder :: String }
    deriving (Eq, Show, IsString)

data ChannelSelection = ChannelSelection
    { chName    :: Channel                  -- recorder channel name
    , chDump    :: Maybe (UdpEvent -> String) -- dump to console
    , chOutput  :: Maybe (Consumer UdpEvent (PS.SafeT IO) ()) -- send output
    }

data AtLimit = Continue | Wrap | Stop
    deriving (Eq, Enum, Bounded, Show)

data ReplayConfig = ReplayConfig
    { cSource       :: Source
    , cRecorder     :: (Name, Recorder)
    , cFileInput    :: Maybe FilePath
    , cT1           :: Maybe UtcTime
    , cT2           :: Maybe UtcTime
    , cAtLimit      :: AtLimit
    , cSpeed        :: Speed
    , cRunning      :: Running
    , cSessionIndex :: Int
    , cChannelSelection :: [ChannelSelection]
    }

speedChoices :: [Speed]
speedChoices = [10 ** (x/10) | x <- [-10..10]]

-- | Pre-fetch buffer size.
prefetch :: Int
prefetch = 1000

-- | Url encode JSON object
objToUrl :: AES.ToJSON a => a -> String
objToUrl = BS8.unpack . HT.urlEncode False . BSL.toStrict . encodeCompact

-- | Make http(s) GET request and perform some action on response.
runGetRequest :: String
    -> (HTC.Response HTC.BodyReader -> IO a)
    -> IO (Either HTC.HttpException a)
runGetRequest uri act = case HTC.parseRequest uri of
    Nothing -> return (Left $ HTC.InvalidUrlException uri "?")
    Just request -> do
        manager <- HTC.newManager $ case HTC.secure request of
            False -> HTC.defaultManagerSettings
            True -> HTCS.tlsManagerSettings
        try $ HTC.withResponse request manager act

-- | Fetch object via GET.
fetchFromRecorder :: AES.FromJSON a => String -> IO a
fetchFromRecorder uri = fix $ \loop -> do
    result <- runGetRequest uri consumer >>= \case
        Left _ -> return Nothing
        Right val -> return $ AES.decodeStrict' val
    maybe (threadDelaySec 1.0 >> loop) return result
  where
    consumer resp = accumulate mempty where
        accumulate x = HTC.responseBody resp >>= \s -> case BS.null s of
            True -> return x
            False -> accumulate (x <> s)

-- | Get starting index, based on utc time.
fetchRecorderStartIndex :: Recorder -> UtcTime -> IO Index
fetchRecorderStartIndex (Recorder rec) t = do
    fetchFromRecorder (rec ++ "/nextIndexFromUtc?t=" ++ fmtTime t)

-- | Get next index.
fetchRecorderNextIndex :: Recorder -> Index -> IO Index
fetchRecorderNextIndex (Recorder rec) ix = do
    fetchFromRecorder (rec ++ "/nextIndexFromIndex?ix=" ++ objToUrl ix)

-- | Fetch events from http(s) to memory buffer.
fetcherRecorder :: Recorder -> UtcTime -> (Either (Vcr.Event ()) UdpEvent -> STM ()) -> Maybe (NEL.NonEmpty Channel) -> IO ()
fetcherRecorder recorder startTime putToBuffer mChannelList = do
    fetchRecorderStartIndex recorder startTime >>= fetchFrom
  where
    -- Stream events to the buffer.
    -- This process will block automatically when buffer gets full.
    -- In case of problems, restart from the last fetched event.
    fetchFrom :: Index -> IO ()
    fetchFrom = fix $ \loop startIndex -> do
        lastIx <- newTVarIO Nothing
        let chSelection = foldr1 (\a b -> a <> "|" <> b) <$> mChannelList
            action resp = runEffect (getData >-> toLines mempty >-> decode >-> save)
              where
                getData :: Producer BS.ByteString IO ()
                getData = do
                    chunk <- liftIO $ HTC.responseBody resp
                    case BS.null chunk of
                        True -> return ()   -- no more data
                        False -> yield chunk >> getData

                toLines :: Functor m => BS.ByteString -> Pipe BS.ByteString BS.ByteString m c
                toLines accumulator = do
                    chunk <- await
                    remaining <- process (accumulator <> chunk)
                    toLines remaining
                  where
                    process buffer = case BS.elemIndex newline buffer of
                        Nothing -> return buffer
                        Just ix -> do
                            let (a,b) = BS.splitAt ix buffer
                            yield a
                            process $ BS.drop 1 b -- drop newline

                decode :: Pipe BS.ByteString (Index, Either (Vcr.Event ()) UdpEvent) IO c
                decode = forever $ do
                    s <- await
                    maybe (return ()) yield (AES.decodeStrict' s)

                save :: Consumer (Index, Either (Vcr.Event ()) UdpEvent) IO c
                save = forever $ do
                    (ix, event) <- await
                    liftIO $ atomically $ do
                        putToBuffer event
                        writeTVar lastIx $ Just ix
        let uri =
                unRecorder recorder
                ++ "/events?includeIndex"
                ++ maybe mempty (\x -> "&ch=" ++ T.unpack x) chSelection
                ++ "&ix=" ++ objToUrl startIndex
        void $ runGetRequest uri action
        threadDelaySec 1.0
        ix <- atomically (readTVar lastIx) >>= \case
            Nothing -> return startIndex
            Just ix -> fetchRecorderNextIndex recorder ix
        loop ix

-- | Fetch events from file to memory buffer.
fetcherFile :: FilePath -> UtcTime
    -> (Either (Vcr.Event ()) UdpEvent -> STM ())
    -> Maybe (NEL.NonEmpty Channel)
    -> IO ()
fetcherFile f startTime putToBuffer mChannelList = do
    fetchStartIndex >>= fetchFrom
  where
    fetchStartIndex = fix $ \loop -> do
        result <- try $ withFile f ReadMode $ \h -> do
            let a = 0
            hSeek h SeekFromEnd 0
            b <- hTell h
            findIndex checkLine startTime h a b
        case (result :: Either IOException (Maybe Integer)) of
            Right (Just ix) -> return ix
            _ -> threadDelaySec 1.0 >> loop

    fetchFrom = fix $ \loop ix -> do
        result <- try $ withFile f ReadMode $ \h -> do
            hSeek h AbsoluteSeek ix
            streamEvents h ix
        threadDelaySec 1.0
        loop $ case result of
            Left (_e :: IOException) -> ix
            Right ix' -> ix'
      where
        streamEvents h = fix $ \loop ix -> hIsEOF h >>= \case
            True -> return ix
            False -> do
                s <- BS8.hGetLine h
                ix' <- hTell h
                case decodeEvent s of
                    -- In case of decode error,
                    -- return index to retry from.
                    -- The problem might be temporary.
                    Left _ -> return ix
                    Right event -> do
                        atomically $ putToBuffer $ case required event of
                            False -> Left (fmap (const ()) event)
                            True -> Right event
                        loop ix'

    required event = case mChannelList of
        Nothing -> True
        Just lst -> eChannel event `elem` lst

    decodeEvent :: BS8.ByteString -> Either String UdpEvent
    decodeEvent = AES.eitherDecodeStrict'

    checkLine s = eTimeUtc <$> decodeEvent s

-- | Select fetcher, depending on configuration
fetcher :: ReplayConfig
    -> UtcTime
    -> (Either (Vcr.Event ()) UdpEvent -> STM ())
    -> Maybe (NEL.NonEmpty Channel)
    -> IO ()
fetcher cfg = case cSource cfg of
    SrcRecorder -> fetcherRecorder (snd $ cRecorder cfg)
    SrcFile -> case cFileInput cfg of
        Nothing -> \_ _ _ -> doNothing
        Just f -> fetcherFile f

-- | Sender process.
-- Use "UTC" time for initial sync, then use monotonic time for actual replay.
sender :: UtcTime
    -> STM (Either (Vcr.Event ()) UdpEvent)
    -> STM ReplayConfig
    -> (String -> STM ())
    -> (UtcTime -> STM ())
    -> [ChannelSelection] -> IO ()
sender startUtcTime getFromBuffer getConfig dumpConsole updateT channelList = do
    -- input queue and process is required for each channel
    realSenders <- forM channelList $ \(ChannelSelection name mDump mOut) -> do
        q <- newTBQueueIO $ intToNatural prefetch
        let consumer = maybe (forever $ await) id mOut
            producer = forever $ do
                event <- liftIO $ atomically $ readTBQueue q
                yield event     -- send via provided consumer
                case mDump of   -- dump to console
                    Nothing -> return ()
                    Just f -> liftIO $ atomically $ dumpConsole $ f event

        return (name, (q, PS.runSafeT $ runEffect $ producer >-> consumer))

    let senderMap = Map.fromList [(name, q) | (name, (q, _)) <- realSenders]

    result <- race (runAll (snd . snd <$> realSenders)) $ do
        (evt, mSp) <- atomically ((,) <$> getFromBuffer <*> getRunningSpeed)
        case mSp of
            Nothing -> notRunning senderMap startUtcTime evt
            Just speed -> startup senderMap startUtcTime speed evt
    case result of
        Left n -> fail $ "sender " ++ show n ++ " failed"
        Right val -> return val

  where

    getRunningSpeed :: STM (Maybe Speed)
    getRunningSpeed = do
        cfg <- getConfig
        return $ bool Nothing (Just $ cSpeed cfg) (cRunning cfg)

    -- We don't want to drop events when switching between notRunning and running modes.
    notRunning senderMap tUtc evt = do
        speed <- atomically (getRunningSpeed >>= maybe retry return)
        startup senderMap tUtc speed evt

    -- Calculate how the virtual time will progress,
    -- depending on current time, speed and pending event.
    startup senderMap tUtc speed evt = do
        tStartupMono <- getMonoTimeNs
        let pendingUtc = either eTimeUtc eTimeUtc evt
            pendingMono = either eTimeMono eTimeMono evt
            deltaTNominal = pendingUtc `Clk.diffUTCTime` tUtc
            deltaTNs = round $ (1000*1000*1000)*toRational deltaTNominal
            t0Mono = pendingMono - deltaTNs
            getVirtualMonotonicTime = do
                dtReal <- (\t -> t - tStartupMono) <$> getMonoTimeNs
                let dt = round (fromIntegral dtReal * speed)
                return (t0Mono + dt)
        running senderMap (either eSessionId eSessionId evt) speed getVirtualMonotonicTime tUtc evt

    -- Timing is only valid within the same session.
    running senderMap session speed getVirtualMonotonicTime = fix $ \loop tUtc evt -> do
        case (either eSessionId eSessionId evt) == session of
            -- new session detected, restart
            False -> startup senderMap tUtc speed evt
            True -> do
                tMono <- getVirtualMonotonicTime
                (tUtc', getNextEvent) <- case tMono >= (either eTimeMono eTimeMono) evt of
                    False -> do
                        threadDelaySec 0.001
                        let deltaT = fromRational (round (speed*1000) % (1000*1000))
                            tUtc' = Clk.addUTCTime deltaT tUtc
                        return (tUtc', pure evt)
                    True -> do
                        either (\_ -> return ()) (fire senderMap) evt
                        let tUtc' = either eTimeUtc eTimeUtc evt
                        return (tUtc', getFromBuffer)
                atomically $ updateT tUtc'
                evt' <- atomically getNextEvent
                mSpeed' <- atomically getRunningSpeed
                case mSpeed' == (Just speed) of
                    False -> case mSpeed' of
                        Nothing -> notRunning senderMap tUtc' evt'
                        Just speed' -> startup senderMap tUtc' speed' evt'
                    True -> do
                        atLimit <- atomically getAtLimit
                        mT2 <- atomically getT2
                        let limitReached = maybe False (\t2 -> tUtc' >= t2) mT2
                        if
                            -- finish sending, caller will restart it from T1
                            | limitReached && atLimit == Wrap -> return ()

                            -- wait until something changes, then recalculate
                            | limitReached && atLimit == Stop -> do
                                atomically $ do
                                    result <- (,) <$> getAtLimit <*> getT2
                                    Control.Monad.when (result == (atLimit, mT2)) $ retry
                                startup senderMap tUtc' speed evt'

                            -- normal case
                            | otherwise -> loop tUtc' evt'

    -- fire event
    fire senderMap evt = case Map.lookup (eChannel evt) senderMap of
        Nothing -> return ()
        Just q -> atomically $ writeTBQueue q evt

    getAtLimit = fmap cAtLimit getConfig

    getT2 = fmap cT2 getConfig

replayEngine ::
    STM ReplayConfig                -- replay configuration
    -> UpdatingVar (Maybe UtcTime)  -- starting Utc time updates
    -> (UtcTime -> STM ())          -- current Utc time updates for GUI
    -> (Maybe String -> STM ())     -- console dump line action
    -> (Int -> STM ())              -- set buffer level indicator, [0..100]
    -> IO String                    -- reason for termination
replayEngine getReplayConfig updatesT0 setTCurrent dumpConsole setBufferLevel = do
    restartOnUpdate updatesT0 $ \tVar -> do
      restartOnChange getReplayConfig compareValue $ \cfg -> do
        atomically $ do
            setBufferLevel 0
            dumpConsole Nothing

        -- Use initial delay, to prevent fast respawn during initial channel
        -- selection (on each channel change, this process will restart).
        -- The same applies for startTime and the recorder.
        -- So just wait until user makes final selection.
        threadDelaySec 1.0

        fix $ \loop -> do
            atomically $ setBufferLevel 0
            let bufferLength = prefetch
            buffer <- newTBQueueIO $ intToNatural bufferLength

            let percent x = (x * 100) `div` bufferLength

                putToBuffer val = do
                    writeTBQueue buffer val
                    lengthTBQueue buffer >>= setBufferLevel . percent . fromEnum

                getFromBuffer = do
                    event <- readTBQueue buffer
                    lengthTBQueue buffer >>= setBufferLevel . percent . fromEnum
                    return event

                updateT t = do
                    writeTVar tVar $ Just t
                    setTCurrent t

            mt <- atomically (readTVar tVar)
            let ch_selection = cChannelSelection cfg
                channelList = do
                    c@(ChannelSelection _channel console out) <- ch_selection
                    guard (isJust console || isJust out)
                    return c
                params = do
                    t <- mt
                    guard $ not $ null channelList
                    return (t, NEL.nonEmpty $ fmap chName ch_selection)
            case params of
                Nothing -> doNothing
                Just (t, channels) -> do
                    result <- race
                        (fetcher cfg t putToBuffer channels)
                        (sender t getFromBuffer getReplayConfig (dumpConsole . Just) updateT channelList)
                    case result of
                        Left _ -> return "fetcher error"
                        Right _ -> do           -- restart from T1
                            atomically $ do
                                t1 <- fmap cT1 getReplayConfig
                                writeTVar tVar t1
                            loop
  where
    compareValue cfg = (src, sessionIx, channels)
      where
        src = case cSource cfg of
            SrcRecorder -> Left $ cRecorder cfg
            SrcFile -> Right $ cFileInput cfg
        sessionIx = cSessionIndex cfg
        channels = do
            ChannelSelection channel console out <- cChannelSelection cfg
            [(channel, isJust console, isJust out)]

-- Date/Time parser YYYY-MM-DD HH:MM:SS[.MM]
pTimeEntry :: MP.Parsec Void String UtcTime
pTimeEntry = do
    datePart <- Cal.fromGregorian
        <$> ML.decimal
        <*> (MC.char '-' *> ML.decimal)
        <*> (MC.char '-' *> ML.decimal)
    void $ MC.char ' '
    timePart <- do
        h <- ML.decimal
        guard $ h < 24
        m <- (MC.char ':' *> ML.decimal )
        guard $ m < 60
        s <- (MC.char ':' *> ML.decimal )
        guard $ s < 62 -- leap seconds
        ms <- MP.try (MC.char '.' *> ML.decimal ) MP.<|> pure 0
        guard $ ms < 1000
        let pico = ((((h * 60) + m) * 60 + s) * 1000 + ms) * 1000 * 1000 * 1000
        return $ Clk.picosecondsToDiffTime pico
    return $ Clk.UTCTime datePart timePart

-- display time in different formats
showTimeEntry :: Clk.UTCTime -> (String, (String, String))
showTimeEntry t =
    let (year, month, day) = Cal.toGregorian $ Clk.utctDay t
        ps = Clk.diffTimeToPicoseconds $ Clk.utctDayTime t
        ms = ps `div` (1000 * 1000 * 1000)
        sec = ms `div` 1000
        hours = sec `div` 3600
        minutes = (sec - hours*3600) `div` 60
        seconds = (sec - hours*3600) `mod` 60
    in
        ( printf "%d-%02d-%02d %02d:%02d:%02d.%03d" year month day hours minutes seconds (ms - (1000 * sec)) :: String
        , ( printf "%d-%02d-%02d\n%02d:%02d:%02d.%03d" year month day hours minutes seconds (ms - (1000 * sec)) :: String
          , printf "%d-%02d-%02d-%02d:%02d:%02d" year month day hours minutes seconds :: String
          )
        )

-- | Replay GUI.
replayGUI ::
    Int
    -> [(Name, Recorder)]
    -> [(Name, Channel {-outputChannelName-} -> Channel {-recorderChannelName-})]
    -> [(Name, [(Channel {-outputChannelName-}, UdpEvent -> String, Consumer UdpEvent (PS.SafeT IO) (), String)])]
    -> IO ()
replayGUI maxDump recorders channelMaps outputs = start gui
  where

    -- highlight wrong time entry
    checkTime w = do
        result <- MP.parseMaybe pTimeEntry <$> get w text
        set w [ bgcolor := maybe red (const white) result ]

    gui = do
        now <- getUtcTime

        replayConfig <- newTVarIO ReplayConfig
            { cSource       = SrcRecorder
            , cRecorder     = recorders !! 0
            , cFileInput    = Nothing
            , cT1           = Just now
            , cT2           = Just now
            , cAtLimit      = minBound
            , cSpeed        = speedChoices !! div (length speedChoices) 2
            , cRunning      = False
            , cSessionIndex = 0
            , cChannelSelection = []
            }

        let updateConfig f = atomically $ modifyTVar replayConfig f

        -- main frame and panel
        f <- frame [ text := "VCR replay" ]
        p <- panel f []

        -- A variable to indicate channel selection change.
        -- It's a workaround (flag) to recalculate channels
        -- and mapping, based on channel map and channel selection.
        channelChange <- variable [ value := False ]

        -- recorder selector
        recorderSelector <- choice p
            [ items := (fmap fst recorders), selection := 0
            , on select ::= \w -> do
                x <- get w selection
                updateConfig $ \cfg -> cfg { cRecorder = (recorders !! x) }
            ]

        -- input file selector
        let defaultFileLabel = "YYYY-MM-DD hh:mm:ss"
        fileLabel <- staticText p [ text := defaultFileLabel ]
        fileSelector <- button p
            [ text := "Select File"
            , on command := do
                current <- fmap (maybe "" id) (atomically $
                    fmap cFileInput (readTVar replayConfig))
                result <- fileOpenDialog f True True "Select recording file"
                    [ ("Recordings",["*.vcr"])
                    , ("Any file", ["*"])
                    ] "" current
                case result of
                    Nothing -> return ()
                    Just filename -> do
                        es <- try $ withFile filename ReadMode $ BS8.hGetLine
                        case es of
                            Left (_e :: IOException) -> return ()
                            Right s -> do
                                let t = case AES.decodeStrict' s of
                                        Nothing -> defaultFileLabel
                                        Just (event :: UdpEvent) ->
                                            snd $ snd $ showTimeEntry $ eTimeUtc event
                                set fileLabel [ text := t ]
                        updateConfig $ \cfg ->
                            cfg { cFileInput = Just filename }
            ]
        set fileSelector [ enabled := False ]

        -- input source
        srcSelector <- radioBox p Vertical
            [ "recorder", "file" ]
            [ text := "source"
            , on select ::= \w -> do
                x <- get w selection
                case x of
                    0 -> do
                        updateConfig $ \c -> c { cSource = SrcRecorder }
                        set recorderSelector [ enabled := True ]
                        set fileSelector     [ enabled := False ]
                    1 -> do
                        updateConfig $ \c -> c { cSource = SrcFile }
                        set recorderSelector [ enabled := False ]
                        set fileSelector     [ enabled := True ]
                    _ -> fail "unexpected selector value"
            ]

        -- channel map selector
        channelMapSelector <- choice p
            [ items := (fmap fst channelMaps), selection := 0
            , on select := set channelChange [ value := True ]
            ]

        -- buffer level gauge
        (bufferLevelVar, bufferLevel) <- (,)
            <$> newTVarIO 0
            <*> hgauge p 100
                [ selection := 0
                , bgcolor := red
                ]

        -- splitter window for ch. selector and console
        sp <- splitterWindow p []
        p1 <- panel sp []
        p2 <- panel sp []

        -- show clock in big fonts
        bigClock <- do
            control <- textCtrlRich p
                [ font := fontFixed { _fontSize = 40 }
                , wrap := WrapNone
                , bgcolor := black
                , textColor := green
                ]
            textCtrlSetEditable control False
            return control

        -- console
        dumpWindow <- do
            control <- textCtrlRich p2
                [ font := fontFixed
                , wrap := WrapNone
                ]
            textCtrlSetEditable control False
            return control

        let timeToolTip = "YYYY-MM-DD HH:MM:SS[.MMM]"

        -- time control t1
        t1 <- textEntry p
            [ tooltip := timeToolTip
            , text := fst $ showTimeEntry now
            , on update ::= \w -> do
                result <- MP.parseMaybe pTimeEntry <$> get w text
                updateConfig $ \c -> c { cT1 = result }
            ]

        -- current time
        (tCurrentVar,tCurrent) <- do
            var <- newUpdatingVarIO (Just now)
            ctr <- textEntry p
                [ tooltip := timeToolTip
                , text := fst $ showTimeEntry now
                , on update ::= \w -> do
                    result <- MP.parseMaybe pTimeEntry <$> get w text
                    atomically $ updateVar var result
                ]
            return (var, ctr)

        -- time control t2
        t2 <- textEntry p
            [ tooltip := timeToolTip
            , text := fst $ showTimeEntry now
            , on update ::= \w -> do
                result <- MP.parseMaybe pTimeEntry <$> get w text
                updateConfig $ \c -> c { cT2 = result }
            ]

        -- 'begin' time button
        t1Button <- button p
            [ text := "begin"
            , on command := do
                s <- get tCurrent text
                set t1 [ text := s ]
                updateConfig $ \c -> c { cT1 = MP.parseMaybe pTimeEntry s }
            ]

        -- 'reset' time button
        t1ButtonApply <- button p
            [ text := "reset"
            , on command := do
                s <- get t1 text
                set tCurrent [ text := s ]
                atomically $ updateVar tCurrentVar $ MP.parseMaybe pTimeEntry s
            ]

        -- 'end' time button
        t2Button <- button p
            [ text := "end"
            , on command := do
                s <- get tCurrent text
                set t2 [ text := s ]
                updateConfig $ \c -> c { cT2 = MP.parseMaybe pTimeEntry s }
            ]

        -- time slider
        tSlider <- hslider p True (-100) 100
            [ selection := 0
            , on mouse ::= \w event -> do
                width <- sizeW <$> get w clientSize
                let x = pointX $ mousePos event
                    sel = round $ ((200 :: Double) * fromIntegral x / fromIntegral width) - 100
                case event of
                    MouseLeftDrag _ _ -> set w [ selection := sel ]
                    MouseLeftDown _ _ -> set w [ selection := sel ]
                    MouseLeftUp _ _ -> set w [ selection := 0 ]
                    _ -> return ()
            ]

        -- limit action selector
        atLimit <- radioBox p Vertical
            (fmap (fmap toLower . show) [(minBound::AtLimit)..maxBound])
            [ text := "at limit"
            , on select ::= \w -> do
                x <- get w selection
                updateConfig $ \c -> c { cAtLimit = toEnum x }
            ]

        -- speed selector
        speedSelector <- do
            let initial = div (length speedChoices) 2
                labels = fmap (printf "x %.02f") speedChoices
            choice p
                [ items := labels
                , selection := initial
                , on select ::= \w -> do
                    ix <- get w selection
                    updateConfig $ \c -> c { cSpeed = speedChoices !! ix }
                ]

        -- run button
        runButton <- toggleButton p
            [ text := "Run", bgcolor := lightgrey
            , on command ::= \w -> do
                x <- get w checked
                updateConfig $ \c -> c { cRunning = x }
                set w [ bgcolor := bool lightgrey green x ]
                set tSlider [ enabled := not x ]
                set tCurrent [ enabled := not x ]
            ]

        -- Unfortunately, the "on click" triggers before the change,
        -- so the actual "notebookGetSelection" must be called later.
        nb <- notebook p1
            [ on click := \_clickPoint -> do
                set channelChange [ value := True ]
                propagateEvent
            ]

        -- console and output selections
        outputPanels <- forM outputs $ \(name, lst) -> do
            cp <- scrolledWindow nb [ scrollRate := sz 20 20 ]

            controls <- forM lst $ \(channel, dump, consumer, outTip) -> do
                enableConsole <- toggleButton cp
                    [ text := "console"
                    , bgcolor := lightgrey
                    , on command ::= \w -> do
                        x <- get w checked
                        set w [ bgcolor := bool lightgrey yellow x ]
                        set channelChange [ value := True ]
                    ]
                enableOutput <- toggleButton cp
                    [ text := "output"
                    , tooltip := outTip
                    , bgcolor := lightgrey
                    , on command ::= \w -> do
                        x <- get w checked
                        set w [ bgcolor := bool lightgrey green x ]
                        set channelChange [ value := True ]
                    ]

                let getDumpFunction = get enableConsole checked >>= \case
                        False -> return Nothing
                        True -> return $ Just dump

                    getOutputConsumer = get enableOutput checked >>= \case
                        False -> return Nothing
                        True -> return $ Just consumer

                    chLayout = widget $ row 5 $
                        [ widget enableConsole
                        , widget enableOutput
                        , label $ T.unpack channel
                        ]

                return (channel, getDumpFunction, getOutputConsumer, chLayout)

            return
                ( name
                , cp
                , column 5 [chLayout | (_,_,_,chLayout) <- controls]
                , [(ch,enConsole,enOutput) | (ch,enConsole,enOutput,_) <- controls]
                )

        consoleMessagesQueue <- newTQueueIO

        -- when the engine is running, it will periodically update time
        tUpdates <- newTVarIO Nothing

        -- replay engine task
        engine <- async $ replayEngine
            (readTVar replayConfig)
            tCurrentVar
            (writeTVar tUpdates . Just)
            (writeTQueue consoleMessagesQueue)
            (writeTVar bufferLevelVar)

        -- menu bar
        mb <- menuBarCreate 0
        fm <- menuCreate "" 0
        menuAppend fm wxID_SAVEAS "&SaveAs" "Save selection to a file" False
        menuAppendSeparator fm
        menuAppend fm wxID_EXIT "&Quit\tCtrl-Q" "Quit application"  False
        _  <- menuBarAppend mb fm "&File"

        frameSetMenuBar f mb

        -- menu exit action
        evtHandlerOnMenuCommand f wxID_EXIT $ close f

        -- menu 'save as' action
        -- The save action might go to a separate function (a lot of code)
        -- however, there are too many arguments to be passed around,
        -- so it's inlined instead.
        evtHandlerOnMenuCommand f wxID_SAVEAS $ do
            cfg <- atomically $ readTVar replayConfig
            let suffix = case cSource cfg of
                    SrcRecorder -> fst (cRecorder cfg)
                    SrcFile -> "file"
                mT1 = cT1 cfg
                mT2 = cT2 cfg
            result <- runExceptT $ do
                -- get time interval
                let timeInterval = do
                        a <- mT1
                        b <- mT2
                        guard $ b > a
                        return (a,b)
                (a,b) <- case timeInterval of
                    Nothing -> do
                        liftIO $ errorDialog f "error" "start/stop time error"
                        throwE ()
                    Just val -> return val

                -- get target file
                targetFile <- do
                    result <- liftIO $ fileSaveDialog f True False "Save selection"
                        [ ("Recordings",["*.vcr"])
                        , ("Any file", ["*.*"])
                        ] "" (snd ( snd $ showTimeEntry a) ++ "-" ++ suffix ++ ".vcr")
                    targetFile <- maybe (throwE ()) pure result
                    exists <- liftIO $ doesPathExist targetFile
                    Control.Monad.when exists $ do
                        yes <- liftIO $ confirmDialog f "confirm" (show targetFile ++ " exists. Overwrite?") False
                        unless yes $ throwE ()
                    return targetFile

                -- get channel selection
                channelSelection <- do
                    d   <- liftIO $ dialog f [text := "Channel selection"]
                    ctr <- liftIO $ radioBox d Vertical
                        [ "session selected channels", "all channels" ]
                        [ text := "channels" ]
                    bCancel <- liftIO $ button d [text := "Cancel"]
                    bOk     <- liftIO $ button d [text := "Ok"]
                    liftIO $ set d [ layout := column 20
                        [ margin 5 $ widget ctr
                        , margin 5 $ row 5
                            [ widget bCancel
                            , widget bOk
                            ]]]
                    result <- liftIO $ showModal d (\fin -> do
                        set bCancel [ on command := fin Nothing ]
                        set bOk [ on command := do
                            sel <- get ctr selection
                            fin (Just sel)]
                        )
                    case result of
                        -- cancel
                        Nothing -> throwE ()
                        -- selected channels only
                        Just 0 -> do
                            let cs = cChannelSelection cfg
                                channelList = do
                                    (ChannelSelection channel console out) <- cs
                                    guard (isJust console || isJust out)
                                    return channel
                            Control.Monad.when (null channelList) $ do
                                liftIO $ errorDialog f "error" "channel list is empty"
                                throwE ()
                            return $ NEL.nonEmpty channelList
                        -- all channels
                        Just 1 -> return Nothing
                        _ -> error $ "internal error, unexpected value: " ++ show result

                return (a, b, targetFile, channelSelection)

            -- perform file save if all selections are correct
            case result of
                Left _ -> return ()
                Right (utc1, utc2, targetFile, channelSelection) -> do
                    -- with open target file
                    ioResult <- try $ withFile targetFile WriteMode $ \h -> do
                        buffer <- newTBQueueIO $ intToNatural prefetch
                        tSaved <- newTVarIO Nothing

                        -- use progress dialog with 'Cancel' button
                        d <- dialog f [text := "Save progress"]
                        lab <- staticText d [text := snd (snd (showTimeEntry utc1)) ]
                        bCancel <- button d [text := "Cancel"]
                        set d
                            [ layout := margin 5 $ column 5
                                [ label "saved:"
                                , widget lab
                                , widget bCancel
                                ]
                            ]
                        set lab [ text := "-" ]

                        let -- save to a file until t2 is reached
                            saveToFile = fix $ \loop -> do
                                (events1, events2) <- atomically $ do
                                    events <- flushTBQueue buffer
                                    case events of
                                        [] -> retry
                                        lst -> return $ span (\evt -> (either eTimeUtc eTimeUtc evt) < utc2) lst
                                case rights events1 of
                                    [] -> return ()
                                    lst -> do
                                        let s = mconcat $ fmap (\evt -> encodeCompact evt <> "\n") lst
                                        BSL.hPut h s
                                        atomically $ writeTVar tSaved $ Just $ eTimeUtc $ last lst
                                case events2 of
                                    [] -> loop
                                    _ -> return ()

                            -- run 'fetcher' and 'saveToFile' actions in parallel
                            -- terminate, when 'saveToFile' is done
                            saveAction = race
                                (fetcher cfg utc1 (writeTBQueue buffer) channelSelection)
                                (saveToFile) >>= \case
                                    Left _ -> fail "event fetcher failed"
                                    Right _ -> return ()

                        -- run saveAction as async, so that a GUI will respond during save
                        withAsync saveAction $ \saveLoop -> showModal d $ \fin -> do
                            let closeDialog val = do
                                    set d [ on idle := return False ]
                                    fin val
                            set bCancel [on command := closeDialog Nothing]
                            set d [on idle := do
                                atomically (readTVar tSaved) >>= \case
                                    Nothing -> return ()
                                    Just x -> set lab [text := snd (snd (showTimeEntry x)) ]
                                poll saveLoop >>= \case
                                    Nothing -> return ()
                                    Just (Left e) -> fail $ show e
                                    Just (Right _) -> do
                                        closeDialog (Just ())
                                -- insert minor delay to prevent looping too fast
                                threadDelaySec 0.05
                                return False
                                ]

                    -- save to file finished
                    case ioResult of
                        Left e -> errorDialog f "error" $ "fatal error: " ++ show (e :: IOException)
                        Right transferResult -> Control.Monad.when (isJust transferResult) $
                            infoDialog f "done" "Transfer finished!"

        -- setup status bar
        status <- statusField [text := "Ready..."]
        set f
            [ statusBar := [ status ]
            , layout := fill $ widget p
            , clientSize := sz 1024 768
            , on closing := do
                cancel engine
                propagateEvent
            ]

        -- set panel layout
        set p [ layout := column 5
            [ hfill $ hrule 1
            , hfill $ row 20
                        [ widget srcSelector
                        , boxed "recorder" $ margin 5 $ widget recorderSelector
                        , boxed "input file" $ margin 5 $ row 5
                            [ widget fileSelector
                            , widget fileLabel
                            ]
                        , boxed "channel map" $ margin 5 $ widget channelMapSelector
                        , boxed "buffer level" $ margin 5 $ widget bufferLevel
                        ]
            , hstretch $ row 5
                [ boxed "UTC time" $ margin 10 $ column 5
                    [ grid 5 5
                        [ [ widget t1Button, minsize (sz 200 (-1)) $ widget t1 ]
                        , [ widget t1ButtonApply, minsize (sz 200 (-1)) $ widget tCurrent]
                        , [ widget t2Button, minsize (sz 200 (-1)) $ widget t2]
                        ]
                    , hfill $ widget tSlider
                    ]
                , widget atLimit
                , boxed "run" $ column 5
                    [ margin 10 $ row 5 [label "speed:", widget speedSelector ]
                    , margin 20 $ widget runButton
                    ]
                , boxed "replay time" $ fill $ widget bigClock
                ]
            , fill $ hsplit sp 5 160
                (widget p1)
                (widget p2)
            ] ]

        -- set p1 layout
        set p1 [ layout := fill $ minsize (sz (-1) 200) $
            tabs nb [ tab name $ container pnl $ empty | (name, pnl, _, _) <- outputPanels] ]
        forM_ outputPanels $ \(_name, pnl, outLayout, _) -> set pnl [ layout := outLayout ]

        -- set p2 layout
        set p2 [ layout := boxed "console output" $ fill $ widget dumpWindow]

        -- run periodic action (checks, updates...)
        void $ timer f [ interval := 100, on command := do

            -- check engine
            poll engine >>= \case
                Nothing -> return ()
                Just x -> do
                    print $ "engine terminated, result: " ++ show x
                    close f

            -- update current time from the engine
            atomically (swapTVar tUpdates Nothing) >>= \case
                Nothing -> return ()
                Just t -> set tCurrent [ text := fst $ showTimeEntry t ]

            -- check time selectors
            checkTime t1
            checkTime tCurrent
            checkTime t2

            -- move current time with a slider
            get tSlider selection >>= \case
                0 -> return ()
                sel -> (MP.parseMaybe pTimeEntry <$> get tCurrent text) >>= \case
                    Nothing -> return ()
                    Just x -> do
                        let k = 10 * ((fromIntegral sel / 100) ^ (7::Int))
                            dt = k * Clk.nominalDay
                            y = Clk.addUTCTime dt x
                        set tCurrent [ text := fst $ showTimeEntry y ]
                        atomically $ updateVar tCurrentVar (Just y)

            -- update channel selection
            get channelChange value >>= \case
                False -> return ()
                True -> do
                    set channelChange [ value := False ]

                    mapper <- do
                        ix <- get channelMapSelector selection
                        return $ snd (channelMaps !! ix)

                    ix <- notebookGetSelection nb
                    let (_,_,_,channels) = outputPanels !! ix
                    lst <- forM channels $ \(ch,toConsole,toOutput) -> do
                        a <- toConsole
                        b <- toOutput
                        return $ ChannelSelection (mapper ch) a b

                    updateConfig $ \c -> c
                        { cSessionIndex = ix
                        , cChannelSelection = lst
                        }

            -- refresh buffer level
            atomically (readTVar bufferLevelVar) >>= \case
                0 -> set bufferLevel
                    [ selection := 0
                    , bgcolor := red
                    ]
                x -> set bufferLevel
                    [ selection := x
                    , bgcolor := lightgrey
                    ]

            -- handle console messages
            do
                messages <- atomically $ flushTQueue consoleMessagesQueue
                forM_ messages $ \case
                    Nothing -> set dumpWindow [ text := "" ]
                    Just msg -> appendText dumpWindow msg
                s <- get dumpWindow text
                let n = length s
                Control.Monad.when (n > 2*maxDump) $ do
                    set dumpWindow [ text := drop (n - maxDump) s ]

            -- update big clock
            do
                mt <- MP.parseMaybe pTimeEntry <$> get tCurrent text
                let s = maybe "" (fst . snd . showTimeEntry) mt
                set bigClock [ text := s ]
            ]


