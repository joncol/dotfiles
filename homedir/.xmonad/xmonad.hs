import Control.Monad (void)
import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import qualified Data.Map as M
import XMonad
import XMonad.Hooks.DynamicBars as DynBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.Spacing
import XMonad.Prompt ( greenXPConfig )
import XMonad.Prompt.Shell ( safePrompt )
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig ( additionalKeys )
import XMonad.Util.Run ( hPutStrLn, spawnPipe )
import XMonad.Util.SpawnOnce ( spawnOnce )

gray = "#7f7f7f"
darkGray = "#3f3f3f"
red = "#900000"
white = "#eeeeee"
flamingoPink = "#f78fb3"

main = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [ D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue ]
  xmonad $ ewmh $ docks (myConfig dbus)

myTerminal = "st -e tmux"
myModMask = mod4Mask

-- [ "", "", "", "", "", "", "", "" ]
myWorkspaces  = [ "\61728"
                , "\61684"
                , "\62056"
                , "\61515"
                , "\61557"
                , "\61749"
                , "\61747"
                , "\61664"
                ]

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig dbus = def
    { terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = 2
    , layoutHook  = avoidStruts $ spacingRaw False
                               (Border 10 0 10 0) True
                               (Border 0 10 0 10) True $
                               Tall 1 (3/100) (1/2) ||| Full
    , manageHook  = manageHook def <+> manageDocks
    , logHook     = dynamicLogWithPP (myLogHook dbus)
    , startupHook = myStartupHook
    , focusedBorderColor = flamingoPink
    , normalBorderColor = "#404040"
    , workspaces  = myWorkspaces
    } `additionalKeys`
    [ ((myModMask .|. shiftMask, xK_x), spawn "slock")
    , ((myModMask, xK_p),               spawn $ "dmenu_run -fn 'Montserrat-12:medium:antialias=true' -x 4 -y 4 -h 27 -dim 0.6 -sf \"" ++ darkGray ++ "\"" ++ " -sb \"" ++ flamingoPink ++ "\"")]

myStartupHook = do
  spawn "~/.local/bin/x-autostart.sh"
  spawn "~/.local/bin/launch_polybar.sh"

myLogHook :: D.Client -> PP
myLogHook dbus = def
  { ppOutput  = dbusOutput dbus
  , ppCurrent = wrap ("%{F" ++ white ++ "} ") " %{F-}"
  , ppVisible = wrap ("%{F" ++ gray ++ "} ") " %{F-}"
  , ppUrgent  = wrap ("%{F" ++ red ++ "} ") " %{F-}"
  , ppHidden  = wrap ("%{F" ++ gray ++ "} ") " %{F-}"
  , ppTitle   = wrap ("%{F" ++ white ++ "} ") " %{F-}"
  , ppLayout  = const ""
  }

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
          D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
