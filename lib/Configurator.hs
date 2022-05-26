{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Recorder configurator GUI application.

module Configurator
    ( runConfigurator
    , module Replay.Types
    , module Vcr
    , module Udp
    , ChannelConfig

    , module Control.Monad
    , module Options.Applicative
    ) where

import           UnliftIO
import           Control.Monad
import           Options.Applicative hiding (command, columns)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time.Clock as Clk
import qualified Data.Time.Calendar as Cal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Text.Printf
import           Lens.Micro.Platform hiding (set)

import           Data.ReactiveValue.Extended

import           Graphics.UI.WXCore
import           Graphics.UI.WX

-- local import
import           Common
import           Replay.Types
import           Vcr
import           Udp
import           Time
import           Streaming.Http
import qualified Capture.Types as CT

data TargetStatus
    = TargetUnknown
    | TargetInSync
    | TargetDifferent
    deriving (Eq, Show)

type ChannelConfig = Map.Map Channel UdpIn

data Target = Target
    { trName :: StaticText ()
    , trPull :: Button ()
    , trPush :: Button ()
    , trDiff :: Button ()
    , trConfigSTM :: TVar (Maybe CT.Config)
    , trConfigRV :: ReactiveFieldReadWrite IO (Maybe CT.Config)
    , trStatusSTM :: TVar (Maybe (Map.Map Channel Bool))
    , trStatusRV :: ReactiveFieldReadWrite IO (Maybe (Map.Map Channel Bool))
    , trTask :: Async ()
    }

showUdpIn :: UdpIn -> String
showUdpIn = \case
    UdpInUnicast ip port -> "Unicast " ++ T.unpack ip ++ ":" ++ T.unpack port
    UdpInMulticast ip port _local -> "Multicast " ++ T.unpack ip ++ ":" ++ T.unpack port

-- | Dump channel configuration as multiline string.
showChannels :: ChannelConfig -> String
showChannels val = flip Map.foldMapWithKey val $ \k a ->
    let ch = T.unpack k
    in ch ++ " <- " ++ showUdpIn a ++ "\n"

-- | Add channel dialog.
addDialog :: ReactiveValueReadWrite a1 (Map.Map Channel UdpIn) IO
    => Window a2 -> a1 -> IO ()
addDialog f var = do
    d <- dialog f [text := "Add channel"]
    channel <- entry d [text := ""]
    cast <- radioBox d Vertical
        [ "multicast", "unicast" ]
        [ text := "mode" ]

    address <- entry d [text := ""]
    port <- entry d [text := ""]
    bCancel <- button d [text := "Cancel"]
    bOk     <- button d [text := "Ok"]
    set d [ layout := column 5
        [ row 5 [ widget channel, label "channel" ]
        , margin 5 $ widget cast
        , row 5 [ widget address, label "address" ]
        , row 5 [ widget port, label "port" ]
        , margin 5 $ row 5
            [ widget bCancel
            , widget bOk
            ]]]
    result <- showModal d (\fin -> do
        set bCancel [ on command := fin Nothing ]
        set bOk [ on command := do
            ch <- T.pack <$> get channel text
            sel <- get cast selection
            t1 <- T.pack <$> get address text
            t2 <- T.pack <$> get port text
            fin (Just (ch, sel, t1, t2))]
        )

    case result of
        Nothing -> return ()
        Just (ch, sel, t1, t2) -> do
            case (ch == "" || t1 == "" || t2 == "") of
                True -> errorDialog f "error" "empty selection error"
                False -> do
                    let udp = case sel of
                            0 -> UdpInMulticast t1 t2 Nothing
                            1 -> UdpInUnicast t1 t2
                            _ -> error "unexpected selection"
                    reactiveValueModify var (Map.insert ch udp)

-- Set list element at given index, taken from 'ilist' package
-- If the index is negative or exceeds list length, the original list will be returned.
setAt :: Int -> a -> [a] -> [a]
setAt i a ls
    | i < 0 = ls
    | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

-- | Remove channel dialog.
removeDialog :: ReactiveValueReadWrite a1 (Map.Map Channel UdpIn) IO
    => Window a2 -> a1 -> IO ()
removeDialog f var = do
    channels <- Map.keys <$> reactiveValueRead var
    chSelection <- newIORef [ False | _ <- channels ]

    d <- dialog f [text := "Remove channels"]

    l  <- listCtrl d
        [ columns := [ ("Channel", AlignLeft, 120) ]
        , items := [[T.unpack ch] | ch <- channels ]
        , on listEvent := \case
            ListItemSelected idx -> modifyIORef chSelection (setAt idx True)
            ListItemDeselected idx -> modifyIORef chSelection (setAt idx False)
            _ -> return ()
        ]

    bCancel <- button d [text := "Cancel"]
    bOk     <- button d [text := "Ok"]
    set d [ layout := column 5
        [ fill $ widget l
        , margin 5 $ row 5
            [ widget bCancel
            , widget bOk
            ]]]
    result <- showModal d (\fin -> do
        set bCancel [ on command := fin Nothing ]
        set bOk [ on command := fin (Just ())]
        )

    case result of
        Nothing -> return ()
        Just _ -> do
            selected <- do
                val <- readIORef chSelection
                return $ Map.fromList (zip channels val)
            let check a False = Just a
                check _ True = Nothing
            reactiveValueModify var $ flip (Map.differenceWith check) selected

-- | Fetch configuration and status from recorder to STM.
targetPolling :: String
    -> (Maybe CT.Config -> STM ())
    -> (Maybe (Map.Map Channel Bool) -> STM ())
    -> IO b
targetPolling addr writeConfig writeStatus = periodic 1.0 $ do
    do  -- poll config
        result <- tryAny $ fetchUrl $ addr ++ "config"
        atomically $ writeConfig $ either (const Nothing) Just result

    do  -- poll status
        result <- tryAny $ fetchUrl $ addr ++ "status"
        atomically $ writeStatus $ either (const Nothing) Just result

-- | Push configuration to the target
pushConfig :: String -> CT.Config -> IO (Either SomeException ())
pushConfig addr conf = tryAny $ putUrl (addr ++ "config") conf

-- | Determine configuration changes
configChanges :: (Eq a, Ord k) =>
    Map.Map k a
    -> Map.Map k a
    -> (Map.Map k a, Map.Map k a, Map.Map k (a, a))
configChanges confX confY = (added, removed, changed)
  where
    added = confX `Map.difference` confY
    removed = confY `Map.difference` confX
    checkF k _v = do
        a <- Map.lookup k confX
        b <- Map.lookup k confY
        guard $ b /= a
        return (b, a)
    changed = Map.mapMaybeWithKey checkF (Map.union confX confY)

-- | Prepare button tooltip
mkTooltip :: Eq a => String -> Map.Map Channel a -> Map.Map Channel a -> String
mkTooltip direction confX confY =
    "changes on " ++ direction
    ++ "\n"
    ++ showChanges "added" added
    ++ showChanges "removed" removed
    ++ showChanges "changed" changed
  where
    (added, removed, changed) = configChanges confX confY
    showChanges chName chMap =
        "\n" ++ chName ++ ": " ++ case fmap T.unpack (Map.keys chMap) of
            [] -> "-"
            t -> foldr1 (\a b -> a ++ ", " ++ b) t

-- | Save configuration to a file
saveAs :: Window a -> ChannelConfig -> IO ()
saveAs f cfg = do
    t <- getUtcTime
    let (year, month, day) = Cal.toGregorian $ Clk.utctDay t
        ps = Clk.diffTimeToPicoseconds $ Clk.utctDayTime t
        ms = ps `div` (1000 * 1000 * 1000)
        sec = ms `div` 1000
        hours = sec `div` 3600
        minutes = (sec - hours*3600) `div` 60
        seconds = (sec - hours*3600) `mod` 60
        s = printf "%04d-%02d-%02d %02d:%02d:%02d" year month day hours minutes seconds :: String
        suggestedFile = "capture-inputs-" ++ s ++ ".json"
            & map (\x -> case x of
                ' ' -> '_'
                c -> c)

    result <- fileSaveDialog f True False "Save configuration"
        [ ("Config",["*.json"])
        , ("Any file", ["*.*"])
        ] "" suggestedFile

    case result of
        Nothing -> return ()
        Just targetFile -> do
            val <- tryAny $ BSL.writeFile targetFile $ encodeJSONPrettyL cfg
            case val of
                Right _ -> return ()
                Left e -> errorDialog f "error" (show e)

-- | Load configuration from a file
loadFile :: Window a -> ReactiveFieldReadWrite IO ChannelConfig -> IO ()
loadFile f channelsRV = do
    result <- fileOpenDialog f True True "Load configuration"
        [ ("Config",["*.json"])
        , ("Any file", ["*"])
        ] "" ""
    case result of
        Nothing -> return ()
        Just filename -> do
            eCfg <- tryAny (BS.readFile filename >>= decodeJSON)
            case eCfg of
                Left e -> errorDialog f "error" (show e)
                Right cfg -> reactiveValueWrite channelsRV cfg

-- | Run configurator GUI.
runConfigurator :: [(Name, String)] -> ChannelConfig -> IO ()
runConfigurator recorders initialConfig = start $ do

    -- local configuration reactive value
    channelsRV <- eqCheck . cbmvarReactiveRW <$> newCBMVar initialConfig

    -- main frame and panel
    f <- frame [ text := "VCR configurator" ]
    p <- panel f []

    -- menu bar
    mb <- menuBarCreate 0
    fm <- menuCreate "" 0
    menuAppend fm wxID_OPEN "&Load" "Load configuration" False
    menuAppend fm wxID_SAVEAS "&SaveAs" "Save configuration" False
    menuAppendSeparator fm
    menuAppend fm wxID_EXIT "&Quit\tCtrl-Q" "Quit application"  False
    _  <- menuBarAppend mb fm "&File"

    frameSetMenuBar f mb

    -- splitter window for recorders and channel selector
    sp <- splitterWindow p []
    p1 <- panel sp []
    p1s <- scrolledWindow p1 [ scrollRate := sz 20 20 ]
    p2 <- panel sp []

    targets <- forM recorders $ \(name, addr) -> do
        cfgSTM <- newTVarIO Nothing
        cfgRV <- eqCheck . cbmvarReactiveRW <$> newCBMVar Nothing
        statSTM  <- newTVarIO Nothing
        statRV <- eqCheck . cbmvarReactiveRW <$> newCBMVar Nothing

        let cfgInputsRV = fmap CT.confInputs <^> cfgRV
            allConfig = liftR2 (,) channelsRV cfgInputsRV

        bPull <- button p1s
            [ text := "Pull"
            , on command := do
                reactiveValueRead cfgRV >>= \case
                    Nothing -> return ()
                    Just val -> reactiveValueWrite channelsRV $ CT.confInputs val
            ]

        bPush <- button p1s
            [ text := "Push"
            , on command := do
                reactiveValueRead cfgRV >>= \case
                    Nothing -> return ()
                    Just cfg -> do
                        newInputs <- reactiveValueRead channelsRV
                        let cfg' = cfg { CT.confInputs = newInputs }
                        proceed <- case Map.null newInputs of
                            True -> proceedDialog f "Confirm" "Upload empty configuration?"
                            False -> return True
                        Control.Monad.when proceed $ pushConfig addr cfg' >>= \case
                            Left e -> errorDialog f "error" (show e)
                            Right _ -> return ()
            ]

        bDiff <- button p1s
            [ text := "Diff"
            , on command := do
                reactiveValueRead allConfig >>= \case
                    (_, Nothing) -> return ()
                    (myConfig, Just otherConfig) -> do
                        let (added, removed, changed) = configChanges myConfig otherConfig
                            pending =
                                "added:\n"
                                ++ showChannels added

                                ++ "\nremoved:\n"
                                ++ showChannels removed

                                ++ "\nchanged:\n"
                                ++ (flip Map.foldMapWithKey changed $ \k (a,b) ->
                                    let ch = T.unpack k
                                    in ch ++ " <-\n"
                                        ++ "  - " ++ showUdpIn a ++ "\n"
                                        ++ "  + " ++ showUdpIn b ++ "\n"
                                    )

                        d <- dialog f [text := "Pending changes"]
                        ds <- scrolledWindow d [ scrollRate := sz 20 20 ]
                        diffWindow <- do
                            ctr <- textCtrlRich ds
                                [ font := fontFixed
                                , wrap := WrapNone
                                , text := pending
                                ]
                            textCtrlSetEditable ctr False
                            return ctr
                        bOk <- button d [text := "Ok"]
                        set ds [ layout := fill $ widget diffWindow ]
                        set d [ layout := column 5
                            [ fill $ minsize (sz 400 400) $ widget ds
                            , margin 5 $ row 5
                                [ widget bOk
                                ]]]
                        void $ showModal d (\fin -> do
                            set bOk [ on command := fin Nothing ]
                            )
            ]

        lab <- staticText p1s [ text := "" ]

        -- colorize/enable buttons, update button tooltips
        follow allConfig $ \(myConfig, mOtherConfig) -> do
            let st
                    | mOtherConfig == Nothing = TargetUnknown
                    | mOtherConfig == Just myConfig = TargetInSync
                    | otherwise = TargetDifferent
                col = case st of
                    TargetUnknown -> red
                    TargetInSync -> green
                    TargetDifferent -> yellow

            let pullTooltip = case mOtherConfig of
                    Nothing -> ""
                    Just otherConfig -> mkTooltip "pull" otherConfig myConfig
                pushTooltip = case mOtherConfig of
                    Nothing -> ""
                    Just otherConfig -> mkTooltip "push" myConfig otherConfig

            set bPull [ bgcolor := col, enabled := (st == TargetDifferent), tooltip := pullTooltip ]
            set bPush [ bgcolor := col, enabled := (st == TargetDifferent), tooltip := pushTooltip ]
            set bDiff [ bgcolor := col, enabled := (st == TargetDifferent) ]

        -- display channel errors
        follow statRV $ \mVal -> do
            let t = case mVal of
                    Nothing -> "unknown"
                    Just val ->
                        let problems = fmap T.unpack $ Map.keys $ Map.filter not val
                        in case problems of
                            [] -> "OK"
                            _ -> "problems : " ++ foldr1 (\a b -> a ++ ", " ++ b) problems
            set lab [ text := (name ++ " (" ++ t ++ ")") ]

        task <- async $ targetPolling addr (writeTVar cfgSTM) (writeTVar statSTM)
        return $ Target lab bPull bPush bDiff cfgSTM cfgRV statSTM statRV task

    addChannel <- button p2
        [ text := "Add"
        , on command := addDialog f channelsRV
        ]

    removeChannel <- button p2
        [ text := "Remove"
        , on command := removeDialog f channelsRV
        ]

    -- channel listing
    chWindow <- do
        ctr <- textCtrlRich p2
            [ font := fontFixed
            , wrap := WrapNone
            , text := ""
            ]
        textCtrlSetEditable ctr False
        return ctr

    follow channelsRV $ \val -> do
        set chWindow [ text := showChannels val ]

    -- setup frame
    set f
        [ layout := fill $ widget p
        , clientSize := sz 1024 768
        ]

    -- set panel layouts
    set p [ layout := column 5
        [ hfill $ hrule 1
        , fill $ hsplit sp 5 160
            (widget p1)
            (widget p2)
        ] ]

    set p1 [ layout := boxed "recorders" $ fill $ minsize (sz (-1) 100) $ widget p1s ]

    set p1s [ layout := column 5 $ do
        tr <- targets
        return $ row 5
            [ widget $ trPull tr
            , widget $ trPush tr
            , widget $ trDiff tr
            , widget $ trName tr
            ]
        ]

    set p2 [ layout := boxed "channels" $ fill $ column 5
        [ hfill $ row 5
            [ widget addChannel
            , widget removeChannel
            ]
        , fill $ widget chWindow
        ] ]

    -- menu exit action
    evtHandlerOnMenuCommand f wxID_EXIT $ close f

    -- menu 'load' action
    evtHandlerOnMenuCommand f wxID_OPEN $ do
        loadFile f channelsRV

    -- menu 'save as' action
    evtHandlerOnMenuCommand f wxID_SAVEAS $ do
        reactiveValueRead channelsRV >>= saveAs f

    -- run periodic action (checks, updates...)
    void $ timer f
        [ interval := 100 -- miliseconds
        , on command := do

            -- for each target...
            forM_ targets $ \tr -> do

                -- check task
                poll (trTask tr) >>= \case
                    Nothing -> return ()
                    Just x -> do
                        print $ "task terrminated, result: " ++ show (x :: Either SomeException ())
                        close f

                -- update STM -> RV
                atomically (readTVar $ trConfigSTM tr)
                    >>= reactiveValueWrite (trConfigRV tr)
                atomically (readTVar $ trStatusSTM tr)
                    >>= reactiveValueWrite (trStatusRV tr)

        ]

