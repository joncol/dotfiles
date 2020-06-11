import Control.Monad (void)
import XMonad
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

main = do
  xmonad $ docks def
    { terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = 2
    , layoutHook  = avoidStruts $ spacingRaw False
                               (Border 10 0 10 0) True
                               (Border 0 10 0 10) True $
                               Tall 1 (3/100) (1/2) ||| Full
    , startupHook = myStartupHook
    , manageHook  = manageHook def <+> manageDocks
    }

myTerminal = "st -e tmux"
myModMask = mod4Mask
myStartupHook = do
  dynStatusBarStartup (\(S sid) -> spawnPipe ("sleep 1 && xmobar -x " ++
                                              show sid ++
                                              " ~/.xmonad/xmobar.hs"))
                      (void (spawnPipe "pkill -sig9 xmobar"))
  spawnOnce "~/.local/bin/x-autostart.sh"
