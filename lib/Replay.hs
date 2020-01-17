
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Replay where

import           Data.Bool
import           Control.Monad
import           Pipes
import qualified Data.Text as T
import           Text.Printf

import           Graphics.UI.WXCore
import           Graphics.UI.WX

import           Control.Concurrent.STM
import           Control.Concurrent.Async

-- local imports
import           Common
import           Event (Channel)

type Name = String
type Uri = String
type Timeout = Double
newtype Recorder = Recorder Uri

data Config = Config
    { confRunning :: Bool
    } deriving (Eq, Show)

data Status = Status

replayEngine :: STM Config -> (Status -> STM ()) -> IO ()
replayEngine getConfig _writeStatus = forever $ do
    void $ atomically getConfig
    threadDelaySec 1.0

-- | Replay GUI.
replayGUI ::
    [(Name, Recorder)]
    -> [(Name, Channel {-outputChannelName-} -> Channel {-recorderChannelName-})]
    -> [(Name, [(Channel {-outputChannelName-}, Timeout, Consumer a IO ())])]
    -> (a -> [String])
    -> IO ()
replayGUI recorders channelMaps outputs _dumpEvent = do
    conf <- newTVarIO $ Config False
    status <- newTVarIO $ Status
    withAsync (replayEngine (readTVar conf) (writeTVar status)) $ \_a -> start gui
  where
    --runReplay selectedRecorder selectedChannelMap selectedOutputs = do undefined
    gui = do
        f <- frame [ text := "VCR replay" ]
        p <- panel f []

        sp <- splitterWindow p []

        p1 <- panel sp []
        p2 <- panel sp []

        -- selectors
        recorderCb <- recorderSelector p
        channelMapCb <- channelMapSelector p

        outputPanels <- forM outputs $ \(_name, lst) -> do
            channelsPanel <- scrolledWindow p1
                [ scrollRate := sz 20 20
                , visible := False
                ]

            controls <- forM lst $ \(channel, _timeout, _consumer) -> do
                chName <- textEntry channelsPanel
                    [ text := T.unpack channel
                    , enabled := False
                    , alignment := AlignRight
                    ]

                -- TODO: use bgcolor to show flow status
                --  - gray: disabled
                --  - green: OK
                --  - red: NOK (timeout)
                enableOutput <- toggleButton channelsPanel
                    [ text := "output"
                    , bgcolor := lightgrey
                    , on command ::= \w -> do
                        x <- get w checked
                        set w [ bgcolor := bool lightgrey green x ]
                    ]
                enableConsole <- toggleButton channelsPanel
                    [ text := "console"
                    , bgcolor := lightgrey
                    , on command ::= \w -> do
                        x <- get w checked
                        set w [ bgcolor := bool lightgrey yellow x ]
                    ]
                return $ row 5 $
                    [ hfill $ widget chName
                    , widget enableOutput
                    , widget enableConsole
                    ]

            let layoutPanel = set channelsPanel
                    [ layout := fill $ column 5 $ controls
                    , visible := True
                    ]

            return (channelsPanel, layoutPanel)

        outputCb <- outputSelector p p1 outputPanels

        {-
        TODO: limit number of entries!
        --textCtrlMakeLogActiveTarget dumpWindow
        -- let dump = logMessage
        -}
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

        -- dump some lines to the dumpWindow
        let dump = appendText dumpWindow
        forM_ [1..30] $ \i -> do
            dump $ "test" ++ show (i::Int)
            dump " test test test...  test test test...  test test test...  test test test...  test test test...  test test test..."
            dump "\n"

        tCurrent <- textEntry p [ tooltip := "YYYY-MM-DD HH:MM:SS.MM" ]
        t1 <- textEntry p [ ]
        set tCurrent
            [ on update := do
                s <- get tCurrent text
                print $ "event..." ++ show s
            ]
        t2 <- textEntry p [ ]

        t1Button <- button p [ text := "start"
            , on command := do {s <- get tCurrent text; set t1 [ text := s ]} ]
        t1ButtonApply <- button p [ text := "from start"
            , on command := do {s <- get t1 text; set tCurrent [ text := s ]} ]
        t2Button <- button p [ text := "end"
            , on command := do {s <- get tCurrent text; set t2 [ text := s ]} ]

        tSlider <- hslider p True (-100) 100 [ selection := 0 ]

        set tSlider
            [ {- on mouse := \event -> do
                dim1 <- get tSlider clientSize
                print (dim1, showMouse event)
                -}
            {-
              on command := do
                i <- get tSlider selection
                print ("command", i)
            -}
            ]

        atLimit <- radioBox p Vertical [ "continue", "wrap", "stop" ] [ text := "at limit" ]


        speedSelector <-
            let speed :: [Float]
                speed = [10 ** (x/10) | x <- [-10..10]]
                labels = fmap (printf "x %.02f") speed
            in choice p
                [ items := labels
                , selection := (div (length speed) 2)
                ]

        runButton <- toggleButton p [ text := "Run" ]

        status <- statusField [text := "Ready..."]
        set f
            [ statusBar := [ status ]
            , layout := container p $ column 5
                [ hfill $ row 20
                        [ boxed "recorder" $ margin 5 $ widget recorderCb
                        , boxed "channel map" $ margin 5 $ widget channelMapCb
                        , boxed "output" $ margin 5 $ widget outputCb
                        ]
                , hfill $ hrule 1
                , boxed "time control" $ hstretch $ margin 10 $ row 5
                    [ boxed "UTC" $ margin 10 $ column 5
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
                    , boxed "virtual time" $ fill $ empty
                    ]
                , fill $ hsplit sp 5 160 (widget p1) (widget p2)
                ]
            , clientSize := sz 800 600
            ]

        set p2 [ layout := boxed "console output" $ fill $ widget dumpWindow]

        redrawChannels p1 outputPanels outputCb

    recorderSelector p = do
        control <- comboBox p
            [ processEnter := True
            , text := "select..."
            ]
        mapM_ (appendText control) (fmap fst recorders)
        return control

    channelMapSelector p = do
        control <- comboBox p
            [ processEnter := True
            , text := "select..."
            ]
        mapM_ (appendText control) (fmap fst channelMaps)
        return control

    redrawChannels parentPanel outputPanels selector = do
        i <- get selector selection
        s <- get selector (item i)
        let (s', _lst) = outputs !! i
        Control.Monad.when (s' /= s) $ fail "internal selector error"

        -- hide all output panels, then make selected output panel visible
        let (selected, layoutPanel) = outputPanels !! i
        forM_ outputPanels $ \(outputPanel, _controls) -> do
            set outputPanel [ visible := False ]

        set parentPanel [ layout := minsize (sz 200 200) $ boxed "channels" $ fill $ widget selected ]
        layoutPanel

    outputSelector panel1 panel2 outputPanels = do
        control <- comboBox panel1
            [ processEnter := True
            , text := "select..."
            , tooltip := "output configuration"
            , on select ::= redrawChannels panel2 outputPanels
            ]
        mapM_ (appendText control) (fmap fst outputs)
        return control

