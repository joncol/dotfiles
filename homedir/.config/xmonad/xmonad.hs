{-# LANGUAGE FlexibleContexts, NamedFieldPuns, PatternGuards #-}

import           Control.Monad
import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import           Data.Default
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Graphics.X11.Xlib.Types ( Rectangle(..) )
import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.UpdatePointer
import           XMonad.Config.Dmwit ( viewShift, withScreen )
import           XMonad.Hooks.DynamicBars as DynBars
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops ( ewmh
                                           , fullscreenEventHook
                                           )
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout
import           XMonad.Layout.Grid
import           XMonad.Layout.IndependentScreens ( countScreens
                                                  , marshallPP
                                                  , onCurrentScreen
                                                  , whenCurrentOn
                                                  , withScreens
                                                  , workspaces'
                                                  )
import           XMonad.Layout.LayoutModifier ( ModifiedLayout )
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig ( additionalKeys )
import           XMonad.Util.Run ( hPutStrLn, safeSpawn, spawnPipe )
import           XMonad.Util.CustomKeys ( customKeys )
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnOnce ( spawnOnce )

gray               = "#7f7f7f"
darkGray           = "#3f3f3f"
red                = "#900000"
white              = "#eeeeee"
flamingoPink       = "#f78fb3"
middleBlue         = "#7ed6df"
coastalBreeze      = "#dff9fb"
hintOfIcePack      = "#c7ecee"
soaringEagle       = "#95afc0"
turbo              = "#f9ca24"
blueberrySoda      = "#7f8fa6"
lightBlueBallerina = "#c8d6e5"

main = do
  countScreens >>= createXmobarPipes
  spawn "killall -q xmobar && sleep 1"
  xmonad . ewmh =<< myStatusBar myConfig

myConfig = def
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = 2
    , layoutHook         = myLayoutHook
    , manageHook         = namedScratchpadManageHook scratchpads <+>
                             myManageHook
    , startupHook        = myStartupHook
    , focusedBorderColor = flamingoPink
    , normalBorderColor  = darkGray
    , keys               = customKeys delKeys insKeys
    , handleEventHook    = docksEventHook
                         <+> handleEventHook def
                         <+> fullscreenEventHook
    , logHook            = updatePointer (0.5, 0.5) (0, 0)
    } `additionalKeys` myKeys
  where
    delKeys = const []
    insKeys = \conf -> let m = modMask conf in
                [ ((m              , xK_Control_L), withScreen 1 W.view)
                , ((m .|. shiftMask, xK_Control_L), withScreen 1 viewShift)
                , ((m              , xK_Alt_L    ), withScreen 0 W.view)
                , ((m .|. shiftMask, xK_Alt_L    ), withScreen 0 viewShift)
                ] ++
                [ ((m .|. e .|. i, key), windows (onCurrentScreen f workspace))
                    | (key, workspace) <- zip [xK_1..xK_9] (workspaces' conf)
                , (e, f) <- [(0, W.view), (shiftMask, viewShift)]
                , i <- [0, controlMask, mod1Mask, controlMask .|. mod1Mask]
                ]

myModMask = mod4Mask

myTerminal = "st -e tmux"

setFullscreenSupported :: X ()
setFullscreenSupported = addSupported [ "_NET_WM_STATE"
                                      , "_NET_WM_STATE_FULLSCREEN"
                                      ]

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
      supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
      changeProperty32 dpy r a aTOM propModeReplace
        (nub $ newSupportedList ++ supportedList)

myStartupHook =
  do
    setFullscreenSupported
    spawn "~/.local/bin/x-autostart.sh"

myLayoutHook =
    fullScreenToggle $ avoidStruts $ mirrorToggle $ mySpacingRaw
      $ lessBorders OnlyScreenFloat
      $   tallLayout
      ||| threeColLayout
      ||| threeColMidLayout
      ||| gridLayout
  where
    mySpacingRaw = spacingRaw
                     False                   -- smartBorder
                     (Border 15 0 15 0) True -- screenBorder
                     (Border 0 15 0 15) True -- windowBorder
    fullScreenToggle = mkToggle (single NBFULL)
    mirrorToggle = mkToggle (single MIRROR)
    tallLayout = Tall 1 (3/100) (1/2)
    threeColLayout = ThreeCol 1 (3/100) (1/2)
    threeColMidLayout = ThreeColMid 1 (3/100) (1/2)
    gridLayout = Grid

myManageHook =
  composeOne
    [ checkDock              -?> doIgnore -- equivalent to manageDocks
    , isDialog               -?> doFloat
    -- , isFullscreen           -?> doFullFloat
    , className =? "Gimp"    -?> doFloat
    , className =? "MPlayer" -?> doFloat
    , return True -?> doF W.swapDown
    ]

myStatusBar conf = do
    screenCount <- countScreens
    return $ docks $ conf
      { workspaces = withScreens screenCount (map show [1..9])
      , startupHook = do
                        (startupHook conf)
                        screenCount <- countScreens
                        refresh
                        mapM_ (spawnPipe . xmobarCommand) [0 .. screenCount-1]
                        docksStartupHook
      , logHook = logHook conf >> myPPs screenCount
      }

myPPs screenCount =
  sequence_ [ dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ (pp s)
            | s <- [0..screenCount-1]
            , pp <- [ppFocus, ppWorkspaces] ]

-- Note that the pipes need to be created with `mkfifo`.
pipeName n s = "/home/jco/.xmonad/pipe-" ++ n ++ "-" ++ show s

createXmobarPipes screenCount =
    sequence_ $ [ spawn $ cmd pn s | pn <- ["focus", "workspaces"]
                                   , s <- [0..screenCount-1] ] ++
                [ spawn "sleep 1" ]
  where
    cmd pn (S s) =
      let filename = pipeName pn s
      in "if [[ ! -p " ++ filename ++ " ]]; then mkfifo " ++ filename ++ "; fi"

ppFocus s@(S s_) = whenCurrentOn s def {
      ppOrder  = \(_:_:windowTitle:_) -> [windowTitle]
    , ppTitle  = color lightBlueBallerina
    , ppLayout = color soaringEagle
    , ppOutput = appendFile (pipeName "focus" s_) . (++"\n")
    }

ppWorkspaces s@(S s_) = marshallPP s defaultPP
    { ppCurrent         = color "white"
    , ppVisible         = color "white"
    , ppHiddenNoWindows = const ""
    , ppUrgent          = const red
    , ppSep             = " | "
    , ppOrder           = \(wss:_layout:_title:_) -> [wss]
    , ppOutput          = appendFile (pipeName "workspaces" s_) . (++"\n")
    }

color c = xmobarColor c ""

xmobarCommand (S s) = unwords [ "xmobar"
                              , "~/.xmonad/xmobar.hs"
                              , "-x"
                              , show s
                              , "-t"
                              , template s
                              , "-C"
                              , pipeReader
                              ]
  where
    template 0 = "%workspaces%}%focus%{%ESGG%"
    template _ = "%workspaces%}%focus%{%date%"
    pipeReader =
      "'[ Run PipeReader \"" ++ pipeName "focus"      s ++ "\" \"focus\"\
       \, Run PipeReader \"" ++ pipeName "workspaces" s ++ "\" \"workspaces\"\
       \]'"

myKeys = let m = myModMask in
    [ ((m .|. shiftMask, xK_x), spawn "slock")
    , ((m, xK_p), do
          rect <- fmap (screenRect . W.screenDetail . W.current)
                       (gets windowset)
          let width = rect_width rect
          spawn $ "dmenu_run -fn 'Montserrat-12:medium:antialias=true' " ++
                  "-h 20 -dim 0.4 -y 2 -sf \"" ++ darkGray ++
                  "\" -sb \"" ++ turbo ++ "\"")
    , ((m, xK_f),                     sendMessage $ Toggle NBFULL)
    , ((m, xK_x),                     sendMessage $ Toggle MIRROR)
    , ((0, xF86XK_AudioLowerVolume ), spawn "~/.local/bin/lower_volume.sh")
    , ((m, xK_F1),                    spawn "~/.local/bin/lower_volume.sh")
    , ((0, xF86XK_AudioRaiseVolume ), spawn "~/.local/bin/raise_volume.sh")
    , ((m, xK_F2),                    spawn "~/.local/bin/raise_volume.sh")
    , ((0, xF86XK_AudioMute ),        spawn "~/.local/bin/mute.sh")
    , ((m, xK_F3),                    spawn "~/.local/bin/mute.sh")
    , ((m, xK_F5),                    spawn "~/.local/bin/screenshot.sh")
    , ((m, xK_F6),                    spawn "~/.local/bin/toggle-screenkey.sh")
    , ((m, xK_minus), namedScratchpadAction scratchpads "telegram")
    , ((m, xK_grave), namedScratchpadAction scratchpads "terminal")
    ] ++
    [ ((m .|. mask, key), f sc)
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
    ]

scratchpads =
  [ NS "telegram" "telegram-desktop" (className =? "TelegramDesktop")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "terminal" ("st -n scratchpad-terminal -e tmux")
         (appName =? "scratchpad-terminal")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]
