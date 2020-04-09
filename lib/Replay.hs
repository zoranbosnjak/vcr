
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Replay where

import           GHC.Natural
import           Data.Maybe
import           Data.Bool
import           Data.Void
import           Control.Monad
import           Control.Exception (SomeException)
import           Control.Monad.Fix
import           Control.Monad.Trans.Except
import           Pipes
import qualified Pipes.Safe as PS
import           Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.Text.Encoding (decodeUtf8)
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
import qualified Data.ByteString.Lazy as BSL
import           Network.HTTP.Req
import qualified Network.HTTP.Client as HTC
import qualified Data.Aeson as AES

-- local imports
import           Common
import           Time
import           Vcr
import           Udp
import           File

type Name = String
type Running = Bool
type Speed = Double

data Recorder
    = RecorderHttp T.Text Int
    | RecorderHttps T.Text Int
    deriving (Eq, Show)

data ChannelSelection = ChannelSelection
    { chName    :: Channel                  -- recorder channel name
    , chDump    :: Maybe (UdpEvent -> String) -- dump to console
    , chOutput  :: Maybe (Consumer UdpEvent (PS.SafeT IO) ()) -- send output
    }

data AtLimit = Continue | Wrap | Stop
    deriving (Eq, Enum, Bounded, Show)

speedChoices :: [Speed]
speedChoices = [10 ** (x/10) | x <- [-10..10]]

-- | Pre-fetch buffer size.
prefetch :: Int
prefetch = 1000

-- | Convert JSON object to Text.
objToText :: AES.ToJSON a => a -> T.Text
objToText = decodeUtf8 . BSL.toStrict . encodeCompact

-- | Make http(s) GET request and perform some action on response.
runGetRequest :: Url scheme -> T.Text -> Option scheme
    -> (HTC.Response HTC.BodyReader -> IO a)
    -> IO (Either HttpException a)
runGetRequest url path opts act = do
    let timeout = responseTimeout $ round $ (1.0::Double) * 1000 * 1000
        request = reqBr GET (url /: path) NoReqBody (opts <> timeout) act
    try $ runReq defaultHttpConfig request

-- | Fetch text via GET.
fetchFromRecorder :: AES.FromJSON a => Url scheme -> T.Text -> Option scheme -> IO a
fetchFromRecorder url path opts = fix $ \loop -> do
    result <- runGetRequest url path opts consumer >>= \case
        Left _ -> return Nothing
        Right val -> return $ AES.decodeStrict' val
    maybe (threadDelaySec 1.0 >> loop) return result
  where
    consumer resp = accumulate mempty where
        accumulate x = HTC.responseBody resp >>= \s -> case BS.null s of
            True -> return x
            False -> accumulate (x <> s)

-- | Get starting index, based on utc time.
fetchStartIndex :: Recorder -> UtcTime -> IO Index
fetchStartIndex recorder startTime = case recorder of
    RecorderHttp  a b -> fetchFromRecorder (http  a) "nextIndexFromUtc" (port b <> "t" =: t)
    RecorderHttps a b -> fetchFromRecorder (https a) "nextIndexFromUtc" (port b <> "t" =: t)
  where
    t = fmtTime startTime

-- | Get next index.
fetchNextIndex :: Recorder -> Index -> IO Index
fetchNextIndex recorder ix = case recorder of
    RecorderHttp  a b -> fetchFromRecorder (http  a) "nextIndexFromIndex" (port b <> "ix" =: objToText ix)
    RecorderHttps a b -> fetchFromRecorder (https a) "nextIndexFromIndex" (port b <> "ix" =: objToText ix)

-- | Fetch events from http(s) to memory buffer.
fetcher :: UtcTime -> (UdpEvent -> STM ()) -> Recorder -> Maybe (NEL.NonEmpty Channel) -> IO ()
fetcher startTime putToBuffer recorder mChannelList = do
    fetchStartIndex recorder startTime >>= fetchFrom
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

                decode :: Pipe BS.ByteString (Index, UdpEvent) IO c
                decode = forever $ do
                    s <- await
                    maybe (return ()) yield (AES.decodeStrict' s)

                save :: Consumer (Index, UdpEvent) IO c
                save = forever $ do
                    (ix, event) <- await
                    liftIO $ atomically $ do
                        putToBuffer event
                        writeTVar lastIx $ Just ix

        void $ case recorder of
            RecorderHttp  a b -> runGetRequest (http  a) "events" (port b <> queryFlag "includeIndex" <> maybe mempty (\lst -> "ch" =: lst) chSelection <> "ix" =: objToText startIndex) action
            RecorderHttps a b -> runGetRequest (https a) "events" (port b <> queryFlag "includeIndex" <> maybe mempty (\lst -> "ch" =: lst) chSelection <> "ix" =: objToText startIndex) action
        threadDelaySec 1.0
        ix <- atomically (readTVar lastIx) >>= \case
            Nothing -> return startIndex
            Just ix -> fetchNextIndex recorder ix
        loop ix

-- | Sender process.
-- Use "UTC" time for initial sync, then use monotonic time for actual replay.
sender :: UtcTime -> STM UdpEvent -> STM (Maybe UtcTime) -> STM AtLimit -> STM Bool
    -> STM Speed -> (String -> STM ()) -> (UtcTime -> STM ()) -> [ChannelSelection] -> IO ()
sender startUtcTime getFromBuffer getT2 getAtLimit getRunning getSpeed dumpConsole updateT channelList = do

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
    getRunningSpeed = getRunning >>= bool (pure Nothing) (fmap Just getSpeed)

    -- We don't want to drop events when switching between notRunning and running modes.
    notRunning senderMap tUtc evt = do
        speed <- atomically (getRunningSpeed >>= maybe retry return)
        startup senderMap tUtc speed evt

    -- Calculate how the virtual time will progress,
    -- depending on current time, speed and pending event.
    startup senderMap tUtc speed evt = do
        tStartupMono <- getMonoTimeNs
        let pendingUtc = eTimeUtc evt
            pendingMono = eTimeMono evt
            deltaTNominal = pendingUtc `Clk.diffUTCTime` tUtc
            deltaTNs = round $ (1000*1000*1000)*toRational deltaTNominal
            t0Mono = pendingMono - deltaTNs
            getVirtualMonotonicTime = do
                dtReal <- (\t -> t - tStartupMono) <$> getMonoTimeNs
                let dt = round (fromIntegral dtReal * speed)
                return (t0Mono + dt)
        running senderMap (eSessionId evt) speed getVirtualMonotonicTime tUtc evt

    -- Timing is only valid within the same session.
    running senderMap session speed getVirtualMonotonicTime = fix $ \loop tUtc evt -> do
        case eSessionId evt == session of
            -- new session detected, restart
            False -> startup senderMap tUtc speed evt
            True -> do
                tMono <- getVirtualMonotonicTime
                (tUtc', getNextEvent) <- case tMono >= eTimeMono evt of
                    False -> do
                        threadDelaySec 0.001
                        let deltaT = fromRational (round (speed*1000) % (1000*1000))
                            tUtc' = Clk.addUTCTime deltaT tUtc
                        return (tUtc', pure evt)
                    True -> do
                        fire senderMap evt
                        let tUtc' = eTimeUtc evt
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

replayEngine ::
    STM Recorder                    -- selected recorder
    -> STM Int                      -- session index
    -> STM [ChannelSelection]       -- replay channels
    -> STM (Maybe UtcTime)          -- t1
    -> UpdatingVar (Maybe UtcTime)  -- starting Utc time updates
    -> STM (Maybe UtcTime)          -- t2
    -> STM AtLimit                  -- what to do, when t reaches t2
    -> STM Speed                    -- selected replay speed
    -> STM Running                  -- running switch
    -> (UtcTime -> STM ())          -- current Utc time updates for GUI
    -> (Maybe String -> STM ())     -- console dump line action
    -> (Int -> STM ())              -- set buffer level indicator, [0..100]
    -> IO String                    -- reason for termination
replayEngine getRecorder getSessionIx getChannelSelections getT1 updatesT0 getT2
  getAtLimit getSpeed getRunning setTCurrent dumpConsole setBufferLevel = do
    restartOnUpdate updatesT0 $ \tVar -> do
      restartOnChange getReplayConfig compareValue $ \(rec, _sesIx, channelList) -> do
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
            let params = do
                    t <- mt
                    guard $ not $ null channelList
                    return (t, NEL.nonEmpty $ fmap chName channelList)
            case params of
                Nothing -> doNothing
                Just (t, channels) -> do
                    result <- race
                        (fetcher t putToBuffer rec channels)
                        (sender t getFromBuffer getT2 getAtLimit getRunning getSpeed (dumpConsole . Just) updateT channelList)
                    case result of
                        Left _ -> return "fetcher error"
                        Right _ -> do           -- restart from T1
                            atomically (getT1 >>= writeTVar tVar)
                            loop

  where
    compareValue (rec, sessionIx, channelList) = (rec, sessionIx, channels)
      where
        channels = do
            ChannelSelection channel console out <- channelList
            return (channel, isJust console, isJust out)

    getReplayConfig :: STM (Recorder, Int, [ChannelSelection])
    getReplayConfig = (,,)
        <$> getRecorder
        <*> getSessionIx
        <*> do
            cs <- getChannelSelections
            let channelList = do
                    c@(ChannelSelection _channel console out) <- cs
                    guard (isJust console || isJust out)
                    return c
            return channelList

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


-- | Replay GUI.
replayGUI ::
    Int
    -> [(Name, Recorder)]
    -> [(Name, Channel {-outputChannelName-} -> Channel {-recorderChannelName-})]
    -> [(Name, [(Channel {-outputChannelName-}, UdpEvent -> String, Consumer UdpEvent (PS.SafeT IO) (), String)])]
    -> IO ()
replayGUI maxDump recorders channelMaps outputs = start gui
  where
    gui = do
        -- main frame and panel
        f <- frame [ text := "VCR replay" ]
        p <- panel f []

        -- A variable to indicate channel selection change.
        -- It's a workaround (flag) to recalculate channels
        -- and mapping, based on channel map and channel selection.
        channelChange <- variable [ value := False ]

        -- recorder selector
        (recorderVar, recorderSelector) <- do
            var <- newTVarIO $ (recorders !! 0)
            ctr <- choice p
                [ items := (fmap fst recorders), selection := 0
                , on select ::= \w -> do
                    x <- get w selection
                    let rec = recorders !! x
                    atomically $ writeTVar var rec
                ]
            return (var, ctr)

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

        now <- getUtcTime

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

        -- time pointers
        ((t1Var,t1), (tCurrentVar,tCurrent), (t2Var,t2)) <- (,,)
            <$> do
                var <- newTVarIO (Just now)
                ctr <- textEntry p
                    [ tooltip := timeToolTip
                    , text := fst $ showTimeEntry now
                    , on update ::= \w -> do
                        result <- MP.parseMaybe pTimeEntry <$> get w text
                        atomically $ writeTVar var result
                    ]
                return (var, ctr)
            <*> do
                var <- newUpdatingVarIO (Just now)
                ctr <- textEntry p
                    [ tooltip := timeToolTip
                    , text := fst $ showTimeEntry now
                    , on update ::= \w -> do
                        result <- MP.parseMaybe pTimeEntry <$> get w text
                        atomically $ updateVar var result
                    ]
                return (var, ctr)
            <*> do
                var <- newTVarIO (Just now)
                ctr <- textEntry p
                    [ tooltip := timeToolTip
                    , text := fst $ showTimeEntry now
                    , on update ::= \w -> do
                        result <- MP.parseMaybe pTimeEntry <$> get w text
                        atomically $ writeTVar var result
                    ]
                return (var, ctr)

        -- 'begin' time button
        t1Button <- button p
            [ text := "begin"
            , on command := do
                s <- get tCurrent text
                set t1 [ text := s ]
                atomically $ writeTVar t1Var $ MP.parseMaybe pTimeEntry s
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
                atomically $ writeTVar t2Var $ MP.parseMaybe pTimeEntry s
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
        (atLimitVar, atLimit) <- do
            var <- newTVarIO minBound
            ctr <- radioBox p Vertical
                (fmap (fmap toLower . show) [(minBound::AtLimit)..maxBound])
                [ text := "at limit"
                , on select ::= \w -> do
                    x <- get w selection
                    atomically $ writeTVar var $ toEnum x
                ]
            return (var, ctr)

        -- speed selector
        (speedVar, speedSelector) <- do
            let initial = div (length speedChoices) 2
            var <- newTVarIO (speedChoices !! initial)
            ctr <-
                let labels = fmap (printf "x %.02f") speedChoices
                in choice p
                    [ items := labels
                    , selection := initial
                    , on select ::= \w -> do
                        ix <- get w selection
                        atomically $ writeTVar var (speedChoices !! ix)
                    ]
            return (var, ctr)

        -- run button
        (runVar, runButton) <- do
            var <- newTVarIO False
            ctr <- toggleButton p
                [ text := "Run", bgcolor := lightgrey
                , on command ::= \w -> do
                    x <- get w checked
                    atomically $ writeTVar var x
                    set w [ bgcolor := bool lightgrey green x ]
                    set tSlider [ enabled := not x ]
                    set tCurrent [ enabled := not x ]
                ]
            return (var, ctr)

        -- Unfortunately, the "on click" triggers before the change,
        -- so the actual "notebookGetSelection" must be called later.
        (nbVar, nb) <- do
            var <- newTVarIO 0
            ctr <- notebook p1
                [ on click := \_clickPoint -> do
                    set channelChange [ value := True ]
                    propagateEvent
                ]
            return (var, ctr)

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

        channelsVar <- newTVarIO []

        consoleMessagesQueue <- newTQueueIO

        -- when the engine is running, it will periodically update time
        tUpdates <- newTVarIO Nothing

        -- replay engine task
        engine <- async $ replayEngine
            (snd <$> readTVar recorderVar)
            (readTVar nbVar)
            (readTVar channelsVar)
            (readTVar t1Var)
            tCurrentVar
            (readTVar t2Var)
            (readTVar atLimitVar)
            (readTVar speedVar)
            (readTVar runVar)
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
            (recName, rec) <- atomically $ readTVar recorderVar
            (mT1, mT2) <- atomically ( (,) <$> readTVar t1Var <*> readTVar t2Var)
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
                        ] "" (snd ( snd $ showTimeEntry a) ++ "-" ++ recName ++ ".vcr")
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
                            cs <- liftIO $ atomically $ readTVar channelsVar
                            let channelList = do
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

                return (a, b, rec, targetFile, channelSelection)

            -- perform file save if all selections are correct
            case result of
                Left _ -> return ()
                Right (utc1, utc2, recorder, targetFile, channelSelection) -> do
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

                        let -- save to a file until t2 reached
                            saveToFile = fix $ \loop -> do
                                (events1, events2) <- atomically $ do
                                    events <- flushTBQueue buffer
                                    case events of
                                        [] -> retry
                                        lst -> return $ span (\evt -> eTimeUtc evt < utc2) lst
                                case events1 of
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
                                (fetcher utc1 (writeTBQueue buffer) recorder channelSelection)
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
                        Left e -> errorDialog f "error" $ "fatal error: " ++ show (e :: SomeException)
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
                        [ boxed "source" $ margin 5 $ widget recorderSelector
                            -- - recorder (recorder selector)
                            -- - file (file select popup)
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

                    atomically $ do
                        writeTVar nbVar ix
                        writeTVar channelsVar lst

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

    -- highlight wrong time entry
    checkTime w = do
        result <- MP.parseMaybe pTimeEntry <$> get w text
        set w [ bgcolor := maybe red (const white) result ]

    -- display time in different formats
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

