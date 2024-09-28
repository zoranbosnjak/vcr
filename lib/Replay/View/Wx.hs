{-# LANGUAGE LambdaCase #-}

-- | This module implements GUI part of VCR replay.

module Replay.View.Wx where

import           UnliftIO
import           Control.Concurrent.STM (flushTQueue)
import           Data.Bool
import           Data.Void
import           Data.ReactiveValue.Extended
import           Data.Char (toLower)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Except
import           Text.Printf
import qualified Data.Text as T
import qualified Data.Time.Clock as Clk
import qualified Data.Time.Calendar as Cal
import           Lens.Micro.Platform hiding (set)
import           System.Directory (doesPathExist)
import           Pipes
import qualified Pipes.Safe as PS

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

import           Graphics.UI.WXCore
import           Graphics.UI.WX
import           Graphics.UI.WX.Reactive.Extended

-- local imports
import           Common
import           Time
import           Vcr
import           Streaming
import           Streaming.Disk
import           Replay.Engine
import           Replay.Types

data SaveToFile
    = AppendFile FilePath
    | ReplaceFile FilePath
    deriving (Eq, Show)

data Buttons act upd evt btn rv1 rv2 lyo = Buttons
    { bChannel          :: Channel
    , bActiveFlag       :: act
    , bUpdateBlinker    :: upd
    , bEnableConsole    :: btn
    , bEnableOutput     :: btn
    , bEnableConsoleRV  :: rv1
    , bEnableOutputRV   :: rv2
    , bChLayout         :: lyo
    }

colorNormal :: Color
colorNormal = lightgrey

colorStandBy :: Color
colorStandBy = yellow

colorActive :: Color
colorActive = green

-- Date/Time parser YYYY-MM-DD HH:MM:SS.MMM
pTimeEntry :: MP.Parsec Void String UtcTime
pTimeEntry = Clk.UTCTime <$> datePart <*> (MC.char ' ' *> timePart)
  where
    nDigits n = (replicateM_ n MC.digitChar) >> MP.notFollowedBy MC.digitChar
    digits n p = MP.lookAhead (nDigits n) >> p

    datePart = Cal.fromGregorian
        <$> digits 4 ML.decimal
        <*> (MC.char '-' *> digits 2 ML.decimal)
        <*> (MC.char '-' *> digits 2 ML.decimal)

    timePart = do
        h :: Int <- digits 2 ML.decimal
        guard $ h < 24
        m :: Int <- MC.char ':' *> digits 2 ML.decimal
        guard $ m < 60
        s :: Int <- (MC.char ':' *> digits 2 ML.decimal)
        ms :: Int <- (MC.char '.' *> digits 3 ML.decimal)
        let seconds :: Double = fromIntegral s + (fromIntegral ms) / 1000
        guard $ seconds < 62 -- leap seconds
        let dt = ((fromIntegral h * 60) + fromIntegral m) * 60 + seconds
            picoSec = dt * 1000 * 1000 * 1000 * 1000
        return $ Clk.picosecondsToDiffTime (round picoSec)

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
        ( printf "%04d-%02d-%02d %02d:%02d:%02d.%03d" year month day hours minutes seconds (ms - (1000 * sec)) :: String
        , ( printf "%04d-%02d-%02d\n%02d:%02d:%02d.%03d" year month day hours minutes seconds (ms - (1000 * sec)) :: String
          , printf "%04d-%02d-%02d %02d:%02d:%02d" year month day hours minutes seconds :: String
          )
        )

-- | UtcTime entry. If unable to parse time entry, return last valid value.
entryUtcTime :: UtcTime -> Control a -> IO (ReactiveFieldReadWrite IO UtcTime)
entryUtcTime t0 ctr = do
    notifiers <- newMVar []
    var <- newMVar t0
    let updateVar = do
            s <- get ctr text
            case MP.parseMaybe pTimeEntry s of
                Nothing -> return ()
                Just t -> void $ swapMVar var t
    let getter = readMVar var
        setter t = do
            set ctr [ text := (fst $ showTimeEntry t) ]
            void $ swapMVar var t
            runNotifiers notifiers
        notifier p = modifyMVar_ notifiers (\x -> return (x ++ [p]))
    set ctr [ on onText :~ \kbd -> kbd >> updateVar >> runNotifiers notifiers ]
    return $ ReactiveFieldReadWrite setter getter notifier

-- | Save recording interval to a file.
saveAs :: Window a -> EngineConfig -> IO ()
saveAs f cfg = do
    tun <- atomically $ cfgTuning cfg

    -- get 'save' arguments from user
    result <- runExceptT $ do
        let t1 = tunMarker1 tun
            t2 = tunMarker2 tun
        unless (t2 > t1) $ do
            liftIO $ errorDialog f "error" "start/stop time error"
            throwE ()

        targetFile <- do
            let suggestedFile = snd ( snd $ showTimeEntry t1) ++ ".vcr"
                    & map (\x -> case x of
                        ' ' -> '_'
                        c -> c)
            result <- liftIO $ fileSaveDialog f True False "Save selection"
                [ ("Recordings",["*.vcr"])
                , ("Any file", ["*.*"])
                ] "" suggestedFile
            targetFile <- maybe (throwE ()) pure result
            exists <- liftIO $ doesPathExist targetFile
            case exists of
                False -> return $ AppendFile targetFile
                True -> do
                    d <- liftIO $ dialog f [text := "File exists."]
                    ctr <- liftIO $ radioBox d Vertical
                        [ "Append file", "Replace file" ]
                        [ text := "action" ]
                    bCancel <- liftIO $ button d [text := "Cancel"]
                    bOk     <- liftIO $ button d [text := "Ok"]
                    liftIO $ set d [ layout := column 20
                        [ margin 5 $ widget ctr
                        , margin 5 $ row 5
                            [ widget bCancel
                            , widget bOk
                            ]]]
                    res <- liftIO $ showModal d (\fin -> do
                        set bCancel [ on command := fin Nothing ]
                        set bOk [ on command := do
                            sel <- get ctr selection >>= \case
                                0 -> return $ AppendFile targetFile
                                1 -> return $ ReplaceFile targetFile
                                _ -> fail "unexpected selection"
                            fin (Just sel)]
                        )
                    case res of
                        Nothing -> throwE ()
                        Just x -> return x

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
                    let channelList = Map.keys $ cfgChannels cfg
                    Control.Monad.when (null channelList) $ do
                        liftIO $ errorDialog f "error" "channel list is empty"
                        throwE ()
                    return $ NEL.nonEmpty channelList
                -- all channels
                Just 1 -> return Nothing
                _ -> error $ "internal error, unexpected value: " ++ show result

        return (t1, t2, targetFile, channelSelection)

    -- perform file save
    case result of
        Left _ -> return ()
        Right (t1, t2, targetFile, channelSelection) -> do
            tSavedSTM <- newTVarIO t1
            tSavedRV <- eqCheck . cbmvarReactiveRW <$> newCBMVar t1

            d <- dialog f [text := "Save progress"]
            lab <- staticText d [text := "YYYY-MM-DD HH:MM:SS" ]
            bCancel <- button d [text := "Cancel"]
            set d
                [ layout := margin 5 $ column 5
                    [ label "saved:"
                    , widget lab
                    , widget bCancel
                    ]
                ]

            follow tSavedRV $ \t -> do
                set lab [ text := (snd $ snd $ showTimeEntry t) ]

            let saveAction :: IO ()
                saveAction = do
                    let player = cfgPlayer cfg
                    ix0 <- fix $ \loop -> do
                        PS.runSafeT (findEventByTimeUtc player t1) >>= \case
                            Nothing -> threadDelaySec 1.0 >> loop
                            Just (i, _event) -> return i

                    let playerFlt = do
                            cs <- channelSelection
                            return (onlyChannels cs, Just 1.0)

                        producer = runPlayer player Vcr.Forward ix0 playerFlt

                        pipesFlt evt = case channelSelection of
                            Nothing -> True -- all channels
                            Just cs -> eChannel evt `elem` cs

                        flt = forever $ do
                            (_ix, evt) <- await
                            Control.Monad.when (pipesFlt evt) $ yield evt

                        endMonitor = fix $ \loop -> do
                            evt <- await
                            let t = eTimeUtc evt
                            atomically $ writeTVar tSavedSTM t
                            case t > t2 of
                                False -> yield evt >> loop
                                True -> return ()

                    targetFile' <- case targetFile of
                            AppendFile fn -> return fn
                            ReplaceFile fn -> do
                                withFile fn WriteMode $ \_ -> return ()
                                return fn
                    let logM _msg = return ()
                        fa = FileArchive TextEncoding targetFile'
                        buffering = Buffering Nothing False
                        recorder = jsonRecorder $ mkFileRecorder buffering fa
                        consumer = recorder logM

                    PS.runSafeT $ runEffect
                        (producer >-> flt >-> endMonitor >-> consumer)

            -- run saveAction as async, so that a GUI will respond during save
            ioResult <- withAsync saveAction $ \saveLoop -> showModal d $ \fin -> do
                let closeDialog val = do
                        set d [ on idle := return False ]
                        fin val
                set bCancel [on command := closeDialog Nothing]
                set d [on idle := do
                    atomically (readTVar tSavedSTM)
                        >>= reactiveValueWrite tSavedRV
                    poll saveLoop >>= \case
                        Nothing -> return ()
                        Just (Left e) -> closeDialog (Just e)
                        Just (Right _) -> closeDialog Nothing
                    threadDelaySec 0.05 -- delay to prevent looping too fast
                    return False
                    ]

            case ioResult of
                Just e -> errorDialog f "error" $ "fatal error: " ++ show e
                Nothing -> do
                    tLast <- atomically (readTVar tSavedSTM)
                    Control.Monad.when (tLast >= t2) $ do
                        infoDialog f "done" "Transfer finished!"

-- | GUI main.
runUI :: TVar UtcTime       -- actual UTC time TVar
    -> Int                  -- console buffer size
    -> [Double]             -- speed choices
    -> [(Name, Source)]     -- event sources
    -> [(Name, Channel -> Channel)] -- channel maps
    -> [(Name, [(Channel, BlinkTime, ConsoleDump, Output)])]    -- outputs
    -> (UI -> IO (ReactiveFieldRead IO EngineConfig, IO ()))    -- controller
    -> Int
    -> IO ()
runUI tUtc maxDump speedChoices sources channelMaps outputs controller periodMs = start $ do
    -- main frame and panel
    f <- frame [ text := "VCR replay" ]
    p <- panel f []

    -- menu bar
    mb <- menuBarCreate 0
    fm <- menuCreate "" 0
    menuAppend fm wxID_SAVEAS "&SaveAs" "Save selection to a file" False
    menuAppendSeparator fm
    menuAppend fm wxID_EXIT "&Quit\tCtrl-Q" "Quit application"  False
    _  <- menuBarAppend mb fm "&File"

    frameSetMenuBar f mb

    -- input source
    srcSelector <- radioBox p Vertical
        [ "recorder", "file" ]
        [ text := "source"
        , selection := 0
        ]

    -- input source reactive value
    srcSelectorRV <- selectorR srcSelector

    -- recorder selector
    recorderSelector <- choice p
        [ items := (fmap fst sources)
        , selection := 0
        ]

    -- input file selector
    selectedFilenameRV <- eqCheck . cbmvarReactiveRW <$> newCBMVar Nothing
    fileSelector <- button p
        [ text := "Select File"
        , on command := do
            current <- fmap (maybe "" id) (reactiveValueRead selectedFilenameRV)
            result <- fileOpenDialog f True True "Select recording file"
                [ ("Recordings",["*.vcr"])
                , ("Any file", ["*"])
                ] "" current
            case result of
                Nothing -> return ()
                Just filename -> do
                    reactiveValueWrite selectedFilenameRV (Just filename)
        ]

    -- enable/disable player selectors
    setEnabled srcSelectorRV recorderSelector UISourceRecorder
    setEnabled srcSelectorRV fileSelector UISourceFile

    -- channel map selector
    channelMapSelector <- choice p
        [ items := (fmap fst channelMaps)
        , selection := 0
        ]

    -- limits display
    limitsSTM <- newTVarIO Nothing
    (limitsRV, limit1, limit2) <- do
        val <- eqCheck . cbmvarReactiveRW <$> newCBMVar Nothing
        let defaultLimitLabel = "YYYY-MM-DD hh:mm:ss"
        limit1 <- staticText p
            [ font := fontFixed
            , text := defaultLimitLabel
            ]
        limit2 <- staticText p
            [ font := fontFixed
            , text := defaultLimitLabel
            ]
        reactiveValueOnCanRead val $ do
            reactiveValueRead val >>= \case
                Nothing -> do
                    set limit1 [ text := defaultLimitLabel ]
                    set limit2 [ text := defaultLimitLabel ]
                Just (a,b) -> do
                    set limit1 [ text := snd (snd (showTimeEntry a)) ]
                    set limit2 [ text := snd (snd (showTimeEntry b)) ]
        return (val, limit1, limit2)

    -- buffer level gauge
    bufferLevelSTM <- newTVarIO 0
    (bufferLevelRV, bufferLevel) <- do
        ctr <- hgauge p 100
            [ selection := 0
            , bgcolor := red
            ]
        val <- eqCheck . cbmvarReactiveRW <$> newCBMVar 0
        reactiveValueOnCanRead val $ do
            reactiveValueRead val >>= \case
                0 -> set ctr
                    [ selection := 0
                    , bgcolor := red
                    ]
                x -> set ctr
                    [ selection := x
                    , bgcolor := lightgrey
                    ]
        return (val, ctr)

    -- time buttons
    t1Button <- button p [ text := "marker 1" ]
    tCButton <- button p [ text := "reset" ]
    t2Button <- button p [ text := "marker 2" ]

    -- time entries
    let tTip = "YYYY-MM-DD HH:MM:SS.MMM"
        colorize ctr = do
            s <- get ctr text
            let c = maybe red (const white) (MP.parseMaybe pTimeEntry s)
            set ctr [ bgcolor := c ]
    now <- atomically $ readTVar tUtc
    t1 <- textEntry p [ tooltip := tTip, text := fst $ showTimeEntry now ]
    tC <- textEntry p [ tooltip := tTip, text := fst $ showTimeEntry now ]
    t2 <- textEntry p [ tooltip := tTip, text := fst $ showTimeEntry now ]

    -- direction selector
    direction <- radioBox p Vertical
        (fmap (fmap toLower . show) [(minBound::Vcr.Direction)..maxBound])
        [ text := "direction" ]
    directionRV <- selectorR direction

    -- Current time updates from controller/engine.
    tFeedbackRVInternal <- eqCheck . cbmvarReactiveRW <$> newCBMVar now

    t1RV <- entryUtcTime now t1
    reactiveValueOnCanRead t1RV (colorize t1)
    buttonClick t1Button >>= \w -> reactiveValueOnCanRead w $ do
        reactiveValueRead tFeedbackRVInternal >>= reactiveValueWrite t1RV

    t2RV <- entryUtcTime now t2
    reactiveValueOnCanRead t2RV (colorize t2)
    buttonClick t2Button >>= \w -> reactiveValueOnCanRead w $ do
        reactiveValueRead tFeedbackRVInternal >>= reactiveValueWrite t2RV

    -- Current time reactive value (set from user interface).
    tCRV <- entryUtcTime now tC
    reactiveValueOnCanRead tCRV (colorize tC)
    buttonClick tCButton >>= \w -> reactiveValueOnCanRead w $ do
        t <- reactiveValueRead directionRV >>= \case
            Vcr.Forward -> reactiveValueRead t1RV
            Vcr.Backward -> reactiveValueRead t2RV
        reactiveValueWrite tCRV t

    -- time slider
    tSliderRV <- eqCheck . cbmvarReactiveRW <$> newCBMVar 0
    tSlider <- hslider p True (-100) 100
        [ selection := 0
        , on mouse ::= \w event -> do
            width <- sizeW <$> get w clientSize
            let x = pointX $ mousePos event
                sel =
                    round $ ((200 :: Double) * fromIntegral x / fromIntegral width) - 100
                    & min 100
                    & max (-100)
            case event of
                MouseLeftDrag _ _ -> do
                    set w [ selection := sel ]
                    reactiveValueWrite tSliderRV sel
                MouseLeftDown _ _ -> do
                    set w [ selection := sel ]
                    reactiveValueWrite tSliderRV sel
                MouseLeftUp _ _ -> do
                    set w [ selection := 0 ]
                    reactiveValueWrite tSliderRV 0
                _ -> return ()
        ]

    -- marker action selector
    atMarker <- radioBox p Vertical
        (fmap (fmap toLower . show) [(minBound::AtMarker)..maxBound])
        [ text := "at marker" ]

    -- speed selector
    speedSelector <- do
        let initial = div (length speedChoices) 2
            labels = fmap (printf "x %.02f") speedChoices
        choice p [ items := labels , selection := initial ]

    -- run button
    runButton <- toggleButton p
        [ text := "Run"
        , on command ::= \w -> do
            x <- get w checked
            set w [ bgcolor := bool lightgrey green x ]
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

    -- auto update clock(s)
    follow tFeedbackRVInternal $ \t -> do
        set tC [ text := (fst $ showTimeEntry t) ]
        set bigClock [ text := (fst $ snd $ showTimeEntry t) ]

    -- console
    dumpWindow <- do
        control <- textCtrlRich p2
            [ font := fontFixed
            , wrap := WrapNone
            ]
        textCtrlSetEditable control False
        return control

    -- Unfortunately, the "on click" triggers BEFORE the change,
    -- so the actual "notebookGetSelection" must be called later.
    -- In this case, the reactive value is updated from periodic action.
    nb <- notebook p1 []
    nbRV <- eqCheck . cbmvarReactiveRW <$> newCBMVar 0

    -- console and output selections
    outputPanels <- forM outputs $ \(name, lst) -> do
        cp <- scrolledWindow nb [ scrollRate := sz 20 20 ]

        controls <- forM lst $ \(channel, blink, consoleDump, out) -> do

            -- A TVar is nested inside another TVar.
            -- In this case, the outer TVar remains the same (a reference)
            -- and the inner TVar can be replaced on each update.
            -- (registerDelay function generates new TVar each time)
            -- This is needed for 'blinker' function.
            activeFlag <- newTVarIO True >>= newTVarIO

            enableConsoleRV <- cbmvarReactiveRW <$> newCBMVar Nothing
            enableConsole <- toggleButton cp
                [ text := "console"
                , bgcolor := colorNormal
                , on command ::= \w -> do
                    x <- get w checked
                    set w [ bgcolor := bool colorNormal colorStandBy x ]
                    let val = bool Nothing (Just consoleDump) x
                    reactiveValueWrite enableConsoleRV val
                ]

            enableOutputRV <- cbmvarReactiveRW <$> newCBMVar Nothing
            enableOutput <- toggleButton cp
                [ text := "output"
                , tooltip := outTooltip out
                , bgcolor := colorNormal
                , on command ::= \w -> do
                    x <- get w checked
                    set w [ bgcolor := bool colorNormal colorStandBy x ]
                    let val = bool Nothing (Just $ outConsumer out) x
                    reactiveValueWrite enableOutputRV val
                ]

            let updateBlinker event = case blink event of
                    Nothing -> return () :: IO ()
                    Just t -> do
                        v <- registerDelay (round $ t * 1000 * 1000)
                        atomically $ writeTVar activeFlag v

                chLayout = widget $ row 5 $
                    [ widget enableConsole
                    , widget enableOutput
                    , label $ T.unpack channel
                    ]

            return $ Buttons
                channel
                activeFlag
                updateBlinker
                enableConsole
                enableOutput
                (readOnly enableConsoleRV)
                (readOnly enableOutputRV)
                chLayout

        return (name, cp, controls)

    -- setup frame
    set f
        [ layout := fill $ widget p
        , clientSize := sz 1024 768
        ]

    -- set panel layout
    set p [ layout := column 5
        [ hfill $ hrule 1
        , hfill $ row 20
                    [ widget srcSelector
                    , boxed "recorder" $ margin 5 $ widget recorderSelector
                    , boxed "input file" $ margin 5 $ widget fileSelector
                    , boxed "channel map" $ margin 5 $ widget channelMapSelector
                    , boxed "limits" $ margin 5 $ column 5
                        [ widget limit1
                        , widget limit2
                        ]
                    , boxed "buffer level" $ margin 5 $ widget bufferLevel
                    ]
        , hstretch $ row 5
            [ boxed "UTC time" $ margin 10 $ column 5
                [ grid 5 5
                    [ [ widget t1Button, minsize (sz 200 (-1)) $ widget t1 ]
                    , [ widget tCButton, minsize (sz 200 (-1)) $ widget tC]
                    , [ widget t2Button, minsize (sz 200 (-1)) $ widget t2]
                    ]
                , hfill $ widget tSlider
                ]
            , column 5
                [ widget direction
                , widget atMarker
                ]
            , boxed "run" $ column 5
                [ margin 10 $ row 5 [label "speed:", widget speedSelector ]
                , margin 20 $ widget runButton
                ]
            , boxed "replay time" $ margin 5 $ fill $ widget bigClock
            ]
        , fill $ hsplit sp 5 160
            (widget p1)
            (widget p2)
        ] ]

    -- set p1 layout
    set p1 [ layout := fill $ minsize (sz (-1) 200) $
        tabs nb [ tab name $ container pnl $ empty | (name, pnl, _) <- outputPanels] ]
    forM_ outputPanels $ \(_name, pnl, controls) -> do
        let outLayout = column 5 (fmap bChLayout controls)
        set pnl [ layout := outLayout ]

    -- set p2 layout
    set p2 [ layout := boxed "console" $ fill $ widget dumpWindow]

    consoleMessagesQueue <- newTQueueIO

    ui <- UI
        <$> pure (close f)
        <*> pure srcSelectorRV
        <*> (liftR (snd . (!!) sources) <$> selectorR recorderSelector)
        <*> pure (readOnly selectedFilenameRV)
        <*> (liftR (snd . (!!) channelMaps) <$> selectorR channelMapSelector)
        <*> pure (writeTVar limitsSTM)
        <*> pure (writeTQueue consoleMessagesQueue)
        <*> pure (writeTVar bufferLevelSTM)
        <*> pure (readOnly tCRV)
        <*> pure (readOnly t1RV)
        <*> pure (readOnly t2RV)
        <*> pure directionRV
        <*> selectorR atMarker
        <*> (liftR ((!!) speedChoices) <$> selectorR speedSelector)
        <*> checkableR runButton
        <*> (liftR (fst . (!!) outputs) <$> pure nbRV)
        <*> pure (do
                (name, _, controls) <- outputPanels
                let lst = do
                        btns <- controls
                        return
                            ( bChannel btns
                            , ( bEnableConsoleRV btns
                              , bEnableOutputRV btns
                              , bUpdateBlinker btns
                              )
                            )
                return (name, lst)
                )

    -- setup reactive relations
    (engineConfigRV, periodicAction) <- controller ui

    -- menu exit action
    evtHandlerOnMenuCommand f wxID_EXIT $ close f

    -- menu 'save as' action
    evtHandlerOnMenuCommand f wxID_SAVEAS $ do
        reactiveValueRead engineConfigRV >>= saveAs f

    -- run periodic action (checks, updates...)
    void $ timer f
        [ interval := periodMs
        , on command := do
            -- auto update clock(s)
            atomically (readTVar tUtc)
                >>= reactiveValueWrite tFeedbackRVInternal

            -- work around notebook selection problem
            notebookGetSelection nb >>= reactiveValueWrite nbRV

            -- move current time with a slider
            reactiveValueRead tSliderRV >>= \case
                0 -> return ()
                n -> do
                    let k = (fromIntegral periodMs / 10) * ((fromIntegral n / 100) ^ (7::Int))
                        dt = k * Clk.nominalDay
                    reactiveValueModify tCRV (Clk.addUTCTime dt)

            -- update limits
            atomically (readTVar limitsSTM)
                >>= reactiveValueWrite limitsRV

            -- update bufferLevel
            atomically (readTVar bufferLevelSTM)
                >>= reactiveValueWrite bufferLevelRV

            -- update button colors
            do
                nbIx <- notebookGetSelection nb
                let (_,_,controls) = outputPanels !! nbIx
                forM_ controls $ \btns -> do
                    col <- atomically $ do
                        var <- readTVar $ bActiveFlag btns -- get inner variable first
                        readTVar var >>= \case      -- then it's value
                            True -> return colorStandBy
                            False -> return colorActive
                    let updateButton w = get w checked >>= \case
                            False -> return ()
                            True -> set w [ bgcolor := col ]
                    updateButton (bEnableConsole btns)
                    updateButton (bEnableOutput btns)

            -- handle console messages
            do
                messages <- atomically $ flushTQueue consoleMessagesQueue
                forM_ messages $ \case
                    ConsoleClear -> set dumpWindow [ text := "" ]
                    ConsolePrint msg -> appendText dumpWindow msg
                s <- get dumpWindow text
                let n = length s
                Control.Monad.when (n > 2*maxDump) $ do
                    set dumpWindow [ text := drop (n - maxDump) s ]

            -- run periodic action from controller
            periodicAction
        ]
