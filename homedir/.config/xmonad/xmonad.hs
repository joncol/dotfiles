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
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.UpdatePointer
import           XMonad.Config.Dmwit ( withScreen )
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
import           XMonad.Util.EZConfig
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
shyMoment          = "#a29bfe"
prunusAvium        = "#e84393"
megaMan            = "#4bcffa"

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
    -- , keys               = \c -> mkKeymap c myKeymap
    , handleEventHook    = docksEventHook
                         <+> handleEventHook def
                         <+> fullscreenEventHook
    , logHook            = updatePointer (0.5, 0.5) (0, 0)
    } `additionalKeysP` myKeys
  where
    delKeys = const []
    insKeys = \conf -> let m = modMask conf in
                [ ((m .|. e, key), windows $ onCurrentScreen f workspace)
                | (key, workspace) <- zip [xK_1..xK_9] (workspaces' conf)
                , (e, f) <- [ (0, W.greedyView)
                            , (shiftMask, W.shift)
                            , (shiftMask .|. controlMask, copy)
                            ]
                ] ++
                [ ((m .|. mask, key), f sc)
                | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                , (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
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
    return ()
    checkKeymap myConfig myKeys
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
                        forM_ [0 .. screenCount-1] $
                           spawnPipe . xmobarCommand screenCount
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

xmobarCommand screenCount (S s) =
    unwords [ "xmobar"
            , "/home/jco/.xmonad/xmobar.hs"
            , "-x"
            , show s
            , "-t"
            , template screenCount s
            , "-C"
            , pipeReader
            ]
  where
    template 1 _ = "%workspaces%}%focus%{" ++
                   "vol:\\ " ++ (font 1 $ color hintOfIcePack "%vol%") ++ sep ++
                   color megaMan "%date%" ++ sep ++
                   color prunusAvium "%uname%"
    template 2 0 = "%workspaces%}%focus%{" ++
                   "vol:\\ " ++ (font 1 $ color hintOfIcePack "%vol%") ++ sep ++
                   color soaringEagle "%ESGG%" ++ sep ++
                   color prunusAvium "%uname%"
    template 2 _ = "%workspaces%}%focus%{" ++ color megaMan "%date%"
    pipeReader =
      "'[ Run PipeReader \"" ++ pipeName "focus"      s ++ "\" \"focus\"\
       \, Run PipeReader \"" ++ pipeName "workspaces" s ++ "\" \"workspaces\"\
       \]'"
    color c msg = "\\<fc=\"" ++ c ++ "\"\\>" ++ msg ++ "\\</fc\\>"
    font fn msg = "\\<fn=" ++ show fn ++ "\\>" ++ msg ++ "\\</fn\\>"
    sep = "\\ \\|\\ "

myKeys =
    [ ("M-S-x", spawn "slock")
    , ("M-p", dmenuRun)
    , ("M-f", sendMessage $ Toggle NBFULL)
    , ("M-x", sendMessage $ Toggle MIRROR)
    , ("<XF86AudioMute>", spawn "~/.local/bin/mute.sh")
    , ("M-<F1>", spawn "~/.local/bin/mute.sh")
    , ("<XF86AudioLowerVolume>", spawn "~/.local/bin/lower_volume.sh")
    , ("M-<F2>", spawn "~/.local/bin/lower_volume.sh")
    , ("<XF86AudioRaiseVolume>", spawn "~/.local/bin/raise_volume.sh")
    , ("M-<F3>", spawn "~/.local/bin/raise_volume.sh")
    , ("M-<F5>", spawn "~/.local/bin/screenshot.sh")
    , ("M-<F6>", spawn "~/.local/bin/toggle-screenkey.sh")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
    , ("M--", namedScratchpadAction scratchpads "telegram")
    , ("M-`", namedScratchpadAction scratchpads "terminal")
    , ("M-S-c", kill1)
    , ("M-v", windows copyToAll)
    , ("M-S-v", killAllOtherCopies)
    ]

scratchpads =
  [ NS "telegram" "telegram-desktop" (className =? "TelegramDesktop")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "terminal" ("st -n scratchpad-terminal -e tmux")
         (appName =? "scratchpad-terminal")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]

dmenuRun = spawn $ "dmenu_run -fn 'Montserrat-12:medium:antialias=true' " ++
                   "-h 20 -dim 0.4 -y 2 -sf \"" ++ darkGray ++
                   "\" -sb \"" ++ turbo ++ "\""
