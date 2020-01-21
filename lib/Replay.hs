
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

import qualified Data.ByteString as BS

-- local imports
import           Common
--import           Time
import           Event (Channel)

type Name = String
type Uri = String
type Speed = Float
newtype Recorder = Recorder Uri deriving (Eq, Show)

data Config = Config
    { confRunning :: Bool
    , confSpeed :: Speed
    , confRecorder :: Recorder
    , confChannels :: [(Channel {- rec.channel -}, Consumer BS.ByteString IO ())]
    }

data Status = Status
    {
    } deriving (Eq, Show)

speedChoices :: [Speed]
speedChoices = [10 ** (x/10) | x <- [-10..10]]

replayEngine :: STM Config -> (Status -> STM ()) -> IO Int
replayEngine getConfig _writeStatus = runAll
    [ fetcher
    , sender
    ]
  where
    fetcher = restartOnChange getCfg $ \(Recorder uri, lst) -> do
        print $ "fetching from: " ++ show uri ++ ", " ++ show lst
        doNothing
      where
        getCfg = getConfig >>= \c -> return (confRecorder c, fst <$> confChannels c)

    sender = restartOnChange (confRunning <$> getConfig) $ \case
        False -> print "sender sleep" >> doNothing
        True -> forever $ do
            cfg <- atomically getConfig
            print (confRunning cfg, confSpeed cfg)
            threadDelaySec 1

-- | Replay GUI.
replayGUI ::
    [(Name, Recorder)]
    -> [(Name, Channel {-outputChannelName-} -> Channel {-recorderChannelName-})]
    -> [(Name, [(Channel {-outputChannelName-}, Consumer a IO ())])]
    -> (a -> [String])
    -> IO ()
replayGUI recorders channelMaps outputs _dumpEvent = start gui
  where
    {-
    tick f engine = do
        poll engine >>= \case
            Nothing -> return ()
            Just x -> do
                print $ "engine terminated, result: " ++ show x
                close f

        _t <- getUtcTime
        return ()
        -- print $ "tick... " ++ show t
    -}

    gui = do
        f <- frame [ text := "VCR replay" ]

        -- No need to call any handler,
        -- IDLE event will be triggered automatically.
        void $ timer f [ interval := 100]

        p <- panel f []

        -- selectors
        recorderSelector <- choice p [ items := (fmap fst recorders), selection := 0 ]
        channelMapSelector <- choice p [ items := (fmap fst channelMaps), selection := 0 ]

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
        {-
        -- dump some lines to the dumpWindow
        let dump = appendText dumpWindow
        forM_ [1..30] $ \i -> do
            dump $ "test" ++ show (i::Int)
            dump " test test test...  test test test...  test test test...  test test test...  test test test...  test test test..."
            dump "\n"
            -}

        -}

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
        t1ButtonApply <- button p [ text := "reset"
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
            let labels = fmap (printf "x %.02f") speedChoices
            in choice p
                [ items := labels
                , selection := (div (length speedChoices) 2)
                ]

        runButton <- toggleButton p [ text := "Run", bgcolor := lightgrey ]

        conf <- newTVarIO =<< Config
            <$> get runButton checked
            <*> (get speedSelector selection >>= \x -> pure (speedChoices !! x))
            <*> (get recorderSelector selection >>= \x -> pure ((fmap snd recorders) !! x))
            <*> pure []

        stat <- newTVarIO $ Status

        set recorderSelector
            [ on select := do
                x <- get recorderSelector selection
                let rec = (fmap snd recorders) !! x
                atomically $ modifyTVar conf $ \val -> val { confRecorder = rec }
            ]

        set runButton
            [ on command := do
                x <- get runButton checked
                atomically $ modifyTVar conf $ \val -> val { confRunning = x }
                set runButton [ bgcolor := bool lightgrey green x ]
            ]

        set speedSelector
            [ on select := do
                x <- get speedSelector selection
                atomically $ modifyTVar conf $ \val -> val { confSpeed = (speedChoices !! x) }
            ]

        nb <- notebook p1 []

        outputPanels <- forM outputs $ \(name, lst) -> do
            cp <- scrolledWindow nb [ scrollRate := sz 20 20 ]

            controls <- forM lst $ \(channel, _consumer) -> do
                enableConsole <- toggleButton cp
                    [ text := "console"
                    , bgcolor := lightgrey
                    , on command ::= \w -> do
                        x <- get w checked
                        set w [ bgcolor := bool lightgrey yellow x ]
                    ]
                enableOutput <- toggleButton cp
                    [ text := "output"
                    , bgcolor := lightgrey
                    , on command ::= \w -> do
                        x <- get w checked
                        set w [ bgcolor := bool lightgrey green x ]
                    ]
                return $ row 5 $
                    [ widget enableConsole
                    , widget enableOutput
                    , label $ T.unpack channel
                    ]

            return (name, cp, column 5 (fmap widget controls))

        engine <- async $ replayEngine (readTVar conf) (writeTVar stat)

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
                -- tick f engine
                return False
                -}
            ]

        set p [ layout := column 5
            [ hfill $ row 20
                        [ boxed "recorder" $ margin 5 $ widget recorderSelector
                        , boxed "channel map" $ margin 5 $ widget channelMapSelector
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
                , boxed "virtual time" $ fill $ empty
                ]
            , fill $ hsplit sp 5 160
                (widget p1)
                (widget p2)
            ] ]

        set p1 [ layout := fill $ minsize (sz (-1) 200) $
            tabs nb [ tab name $ container pnl $ empty | (name, pnl, _) <- outputPanels] ]
        forM_ outputPanels $ \(_name, pnl, outLayout) -> set pnl [ layout := outLayout ]

        set p2 [ layout := boxed "console output" $ fill $ widget dumpWindow]

