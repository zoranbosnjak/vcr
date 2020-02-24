
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Replay where

import           GHC.Natural
import           Data.Maybe
import           Data.Bool
import           Data.Void
import           Control.Monad
import           Control.Monad.Fix
import           Pipes
import           Data.Function ((&))
import           Data.Ratio ((%))
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Char (toLower)
import           Text.Printf
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Data.Time.Clock as Clk
import qualified Data.Time.Calendar as Cal

import           Graphics.UI.WXCore
import           Graphics.UI.WX

import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception (try)


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
--import qualified Data.ByteString.Lazy as BSL
import           Network.HTTP.Req
import           Text.URI (mkURI)
import qualified Network.HTTP.Client as HTC
import qualified Data.Aeson as AES

-- local imports
import           Common
import           Time
import           Event
import           Vcr

type Name = String
type Running = Bool
type Speed = Double

newtype Recorder = Recorder T.Text deriving (Eq, Show)

data ChannelSelection = ChannelSelection
    { chName    :: Channel                  -- recorder channel name
    , chDump    :: Bool                     -- dump to console
    , chOutput  :: Maybe (Consumer BS.ByteString IO ()) -- send output
    }

data AtLimit = Continue | Wrap | Stop
    deriving (Eq, Enum, Bounded, Show)

speedChoices :: [Speed]
speedChoices = [10 ** (x/10) | x <- [-10..10]]

-- | Pre-fetch buffer size per channel.
prefetch :: Int
prefetch = 100

-- | Fetch events from http(s) to buffer
fetcher :: Maybe UtcTime -> (UdpEvent -> STM ()) -> Recorder -> [Channel] -> IO ()
fetcher _startTime _putToBuffer (Recorder _rec) [] = doNothing
fetcher Nothing _putToBuffer (Recorder _rec) _channelList = doNothing
fetcher (Just startTime) putToBuffer (Recorder rec) channelList = do
    let to = responseTimeout $ round $ (1.0::Double) * 1000 * 1000

    -- get nextIndex from start utc time
    startIx <- case (mkURI (rec <> "/nextIndex") >>= useURI) of
        Nothing -> do
            doNothing
        Just result -> do
            let request = case result of
                    Left  (url, opt) -> req GET url NoReqBody bsResponse (opt <> to <> ("t" =: (fmtTime startTime)))
                    Right (url, opt) -> req GET url NoReqBody bsResponse (opt <> to <> ("t" =: (fmtTime startTime)))
            fix $ \loop -> (try $ runReq defaultHttpConfig $ fmap responseBody request) >>= \case
                Left (_error :: HttpException) -> threadDelaySec 1.0 >> loop
                Right val -> return $ decodeUtf8 val

    -- Stream events to the buffer.
    -- This process will block automatically when buffer gets full.
    case (mkURI (rec <> "/events") >>= useURI) of
        Nothing -> doNothing
        Just result -> do
            let cl = foldr1 (\a b -> a <> "|" <> b) channelList
                consumer resp = do
                    let getChunk = HTC.responseBody resp
                        go accumulator = getChunk >>= \s -> case BS.null s of
                            True -> return ()
                            False -> do
                                s' <- (accumulator <> s) & (fix $ \loop x -> do
                                    let (a,b) = BS8.break (== '\n') x
                                    case AES.decodeStrict a of
                                        Nothing -> return $ BS.drop 1 b
                                        Just val -> do
                                            -- TODO: do not drop if not a complete frame is received!!
                                            atomically $ putToBuffer (val :: UdpEvent)
                                            loop $ BS.drop 1 b
                                    )
                                go s'
                    go mempty
                request = case result of
                    Left  (url, opt) -> reqBr GET url NoReqBody (opt <> to <> ("ch" =: cl) <> ("t" =: startIx)) consumer
                    Right (url, opt) -> reqBr GET url NoReqBody (opt <> to <> ("ch" =: cl) <> ("t" =: startIx)) consumer
            -- TODO: in case of problems, restart request from the last good index
            -- (_result :: Either HttpException ()) <- try $ runReq defaultHttpConfig request
            runReq defaultHttpConfig request

-- | Sender process.
-- Use "UTC" time for initial sync, then use monotonic time for actual replay.
sender :: Maybe UtcTime -> STM UdpEvent -> STM Bool -> STM Speed -> (UtcTime -> STM ()) -> IO UtcTime
sender Nothing _getFromBuffer _getRunning _getSpeed _setTCurrent = doNothing
sender (Just startUtcTime) getFromBuffer getRunning getSpeed updateT = do
    (evt, mSp) <- atomically ((,) <$> getFromBuffer <*> getRunningSpeed)
    case mSp of
        Nothing -> notRunning startUtcTime evt
        Just speed -> startup startUtcTime speed evt
  where
    getRunningSpeed :: STM (Maybe Speed)
    getRunningSpeed = getRunning >>= bool (pure Nothing) (fmap Just getSpeed)

    -- We don't want to drop events when switching between notRunning and running modes.
    notRunning tUtc evt = do
        speed <- atomically (getRunningSpeed >>= maybe retry return)
        startup tUtc speed evt

    -- Calculate how the virtual time will progress,
    -- depending on current time, speed and pending event.
    startup tUtc speed evt = do
        tStartupMono <- getMonoTimeNs
        let pendingUtc = eTimeWall evt
            pendingMono = eTimeMono evt
            deltaTNominal = pendingUtc `Clk.diffUTCTime` tUtc
            deltaTNs = round $ (1000*1000*1000)*toRational deltaTNominal
            t0Mono = pendingMono - deltaTNs
            getVirtualMonotonicTime = do
                dtReal <- (\t -> t - tStartupMono) <$> getMonoTimeNs
                let dt = round (fromIntegral dtReal * speed)
                return (t0Mono + dt)
        running (eSessionId evt) speed getVirtualMonotonicTime tUtc evt

    -- Timing is only valid within the same session.
    running session speed getVirtualMonotonicTime = fix $ \loop tUtc evt -> do
        case eSessionId evt == session of
            -- new session detected, restart
            False -> startup tUtc speed evt
            True -> do
                tMono <- getVirtualMonotonicTime
                (tUtc', getNextEvent) <- case tMono >= eTimeMono evt of
                    False -> do
                        threadDelaySec 0.001
                        let deltaT = fromRational (round (speed*1000) % (1000*1000))
                            tUtc' = Clk.addUTCTime deltaT tUtc
                        return (tUtc', pure evt)
                    True -> do
                        fire evt
                        let tUtc' = eTimeWall evt
                        return (tUtc', getFromBuffer)
                atomically $ updateT tUtc'
                evt' <- atomically getNextEvent
                mSpeed' <- atomically getRunningSpeed
                case mSpeed' /= (Just speed) of
                    False -> loop tUtc' evt'
                    True -> case mSpeed' of
                        Nothing -> notRunning tUtc' evt'
                        Just speed' -> startup tUtc' speed' evt'

    fire evt = print (eChannel evt, eTimeWall evt)

replayEngine ::
    STM Recorder                    -- selected recorder
    -> STM [ChannelSelection]       -- replay channels
    -> STM (Maybe UtcTime)          -- t1
    -> UpdatingVar (Maybe UtcTime)  -- starting Utc time updates
    -> STM (Maybe UtcTime)          -- t2
    -> STM AtLimit                  -- what to do, when t reaches t2
    -> STM Speed                    -- selected replay speed
    -> STM Running                  -- running switch
    -> (UtcTime -> STM ())          -- current Utc time updates for GUI
    -> (T.Text -> STM ())           -- console dump line action
    -> (Int -> STM ())              -- set buffer level indicator, [0..100]
    -> IO ()
replayEngine getRecorder getChannelSelections getT1 updatesT0 _getT2
  _getAtLimit getSpeed getRunning setTCurrent _dumpConsole setBufferLevel = do
    restartOnUpdate updatesT0 $ \tVar -> do
      restartOnChange getReplayConfig $ \(rec, channelList) -> do
        atomically $ setBufferLevel 0
        -- Use initial delay, to prevent fast respawn during initial channel
        -- selection (on each channel change, this process will restart).
        -- The same applies for startTime and the recorder.
        -- So just wait until user makes final selection.
        threadDelaySec 1.0

        fix $ \loop -> do
            atomically $ setBufferLevel 0
            let bufferLength = length channelList * prefetch

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

            t <- atomically $ readTVar tVar
            result <- race
                (fetcher t putToBuffer rec channelList)
                (sender t getFromBuffer getRunning getSpeed updateT)
            case result of
                Left _ -> return ()     -- fetcher stopped, something went wrong
                Right _ -> do           -- restart from T1
                    atomically (getT1 >>= writeTVar tVar)
                    loop

  where
    getReplayConfig = (,)
        <$> getRecorder
        <*> do
            cs <- getChannelSelections
            let channelList = do
                    ChannelSelection channel console out <- cs
                    guard (console || isJust out)
                    return channel
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
        m <- (MC.char ':' *> ML.decimal )
        s <- (MC.char ':' *> ML.decimal )
        ms <- MP.try (MC.char '.' *> ML.decimal ) MP.<|> pure 0
        let pico = ((((h * 60) + m) * 60 + s) * 1000 + ms) * 1000 * 1000 * 1000
        return $ Clk.picosecondsToDiffTime pico
    return $ Clk.UTCTime datePart timePart


-- | Replay GUI.
replayGUI ::
    [(Name, Recorder)]
    -> [(Name, Channel {-outputChannelName-} -> Channel {-recorderChannelName-})]
    -> [(Name, [(Channel {-outputChannelName-}, Consumer BS.ByteString IO ())])]
    -> (a -> [String])
    -> IO ()
replayGUI recorders channelMaps outputs _dumpEvent = start gui
  where

    gui = do

        f <- frame [ text := "VCR replay" ]

        p <- panel f []

        -- A variable to indicate channel selection change.
        -- It's a workaround (flag) to recalculate channels
        -- and mapping, based on channel map and channel selection.
        channelChange <- variable [ value := False ]

        -- recorder selector
        (recorderVar, recorderSelector) <- do
            var <- newTVarIO $ snd (recorders !! 0)
            ctr <- choice p
                [ items := (fmap fst recorders), selection := 0
                , on select ::= \w -> do
                    x <- get w selection
                    let rec = (fmap snd recorders) !! x
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

        sp <- splitterWindow p []
        p1 <- panel sp []
        p2 <- panel sp []

        {-
        TODO: limit number of entries!
        --textCtrlMakeLogActiveTarget dumpWindow
        -- let dump = logMessage
        -}
        -- dumpWindow <- button p2 [ text := "test2" ]
        dumpWindow <- do
            control <- textCtrlRich p2
                [ font := fontFixed -- { _fontSize = 12 }
                -- , bgcolor := black
                -- , textColor := red
                , wrap := WrapNone
                -- , enabled := False
                ]
            textCtrlSetEditable control False
            return control

        {-
        -- dump some lines to the dumpWindow
        let dump = appendText dumpWindow
        forM_ [1..30] $ \i -> do
            dump $ "test" ++ show (i::Int)
            dump " test test test...  test test test...  test test test...  test test test...  test test test...  test test test..."
            dump "\n"
        -}

        let timeToolTip = "YYYY-MM-DD HH:MM:SS[.MMM]"

        ((t1Var,t1), (tCurrentVar,tCurrent), (t2Var,t2)) <- getUtcTime >>= \now -> (,,)
            <$> do
                var <- newTVarIO (Just now)
                ctr <- textEntry p
                    [ tooltip := timeToolTip
                    , text := showTimeEntry now
                    , on update ::= \w -> do
                        result <- MP.parseMaybe pTimeEntry <$> get w text
                        atomically $ writeTVar var result
                    ]
                return (var, ctr)
            <*> do
                var <- newUpdatingVarIO (Just now)
                ctr <- textEntry p
                    [ tooltip := timeToolTip
                    , text := showTimeEntry now
                    , on update ::= \w -> do
                        result <- MP.parseMaybe pTimeEntry <$> get w text
                        atomically $ updateVar var result
                    ]
                return (var, ctr)
            <*> do
                var <- newTVarIO (Just now)
                ctr <- textEntry p
                    [ tooltip := timeToolTip
                    , text := showTimeEntry now
                    , on update ::= \w -> do
                        result <- MP.parseMaybe pTimeEntry <$> get w text
                        atomically $ writeTVar var result
                    ]
                return (var, ctr)

        t1Button <- button p
            [ text := "start"
            , on command := do
                s <- get tCurrent text
                set t1 [ text := s ]
                atomically $ writeTVar t1Var $ MP.parseMaybe pTimeEntry s
            ]

        t1ButtonApply <- button p
            [ text := "reset"
            , on command := do
                s <- get t1 text
                set tCurrent [ text := s ]
                atomically $ updateVar tCurrentVar $ MP.parseMaybe pTimeEntry s
            ]

        t2Button <- button p
            [ text := "end"
            , on command := do
                s <- get tCurrent text
                set t2 [ text := s ]
                atomically $ writeTVar t2Var $ MP.parseMaybe pTimeEntry s
            ]

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

        (runVar, runButton) <- do
            var <- newTVarIO False
            ctr <- toggleButton p
                [ text := "Run", bgcolor := lightgrey
                , on command ::= \w -> do
                    x <- get w checked
                    atomically $ writeTVar var x
                    set w [ bgcolor := bool lightgrey green x ]
                    set tSlider [ enabled := not x ]
                    set t1ButtonApply [ enabled := not x ]
                    set tCurrent [ enabled := not x ]
                ]
            return (var, ctr)

        -- Unfortunately, the "on click" triggers before the change,
        -- so the actual "notebookGetSelection" must be called later.
        nb <- notebook p1
            [ on click := \_clickPoint -> do
                set channelChange [ value := True ]
                propagateEvent
            ]

        outputPanels <- forM outputs $ \(name, lst) -> do
            cp <- scrolledWindow nb [ scrollRate := sz 20 20 ]

            controls <- forM lst $ \(channel, consumer) -> do
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
                    , bgcolor := lightgrey
                    , on command ::= \w -> do
                        x <- get w checked
                        set w [ bgcolor := bool lightgrey green x ]
                        set channelChange [ value := True ]
                    ]

                let isConsoleEnabled = get enableConsole checked

                    getOutputConsumer = get enableOutput checked >>= \case
                        False -> return Nothing
                        True -> return $ Just consumer

                    chLayout = widget $ row 5 $
                        [ widget enableConsole
                        , widget enableOutput
                        , label $ T.unpack channel
                        ]

                return (channel, isConsoleEnabled, getOutputConsumer, chLayout)

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

        engine <- async $ replayEngine
            (readTVar recorderVar)
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

        status <- statusField [text := "Ready..."]
        set f
            [ statusBar := [ status ]
            , layout := fill $ widget p
            , clientSize := sz 1024 768
            , on closing := do
                cancel engine
                propagateEvent
            {-
            , on idle := do
                tick f engine
                return False
            -}
            ]

        set p [ layout := column 5
            [ hfill $ row 20
                        [ boxed "recorder" $ margin 5 $ widget recorderSelector
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
                , boxed "replay time" $ fill $ empty
                ]
            , fill $ hsplit sp 5 160
                (widget p1)
                (widget p2)
            ] ]

        set p1 [ layout := fill $ minsize (sz (-1) 200) $
            tabs nb [ tab name $ container pnl $ empty | (name, pnl, _, _) <- outputPanels] ]
        forM_ outputPanels $ \(_name, pnl, outLayout, _) -> set pnl [ layout := outLayout ]

        set p2 [ layout := boxed "console output" $ fill $ widget dumpWindow]

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
                Just t -> set tCurrent [ text := showTimeEntry t ]

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
                        set tCurrent [ text := showTimeEntry y ]
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

                    atomically $ writeTVar channelsVar lst

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
            ]

    checkTime w = do
        result <- MP.parseMaybe pTimeEntry <$> get w text
        set w [ bgcolor := maybe red (const white) result ]

    showTimeEntry t =
        let (year, month, day) = Cal.toGregorian $ Clk.utctDay t
            ps = Clk.diffTimeToPicoseconds $ Clk.utctDayTime t
            ms = ps `div` (1000 * 1000 * 1000)
            sec = ms `div` 1000
            hours = sec `div` 3600
            minutes = (sec - hours*3600) `div` 60
            seconds = (sec - hours*3600) `mod` 60
        in printf "%d-%02d-%02d %02d:%02d:%02d.%03d" year month day hours minutes seconds (ms - (1000 * sec))

