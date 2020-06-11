import Control.Monad (void)
import XMonad
import XMonad.Hooks.DynamicBars as DynBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.Spacing
import XMonad.Prompt ( greenXPConfig )
import XMonad.Prompt.Shell ( safePrompt )
import XMonad.Util.EZConfig ( additionalKeys )
import XMonad.Util.Run ( hPutStrLn, spawnPipe )
import XMonad.Util.SpawnOnce ( spawnOnce )

main = do
  spawnPipe "pkill -sig9 xmobar"
  n <- countScreens
  xmprocs <- mapM (\i -> spawnPipe $ "sleep 1 && xmobar -x " ++ show i ++
                                     " /home/jco/.xmonad/xmobar.hs")
               [0..n-1]
  xmonad $ docks (myConfig xmprocs)

myTerminal = "st -e tmux"
myModMask = mod4Mask

-- [ "", "", "", "", "", "", "", "" ]
myWorkspaces  = [ "\xf269"
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

myConfig xmprocs = def
    { terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = 2
    , layoutHook  = avoidStruts $ spacingRaw False
                               (Border 10 0 10 0) True
                               (Border 0 10 0 10) True $
                               Tall 1 (3/100) (1/2) ||| Full
    , manageHook  = manageHook def <+> manageDocks
    , logHook     = mapM_ (\h -> dynamicLogWithPP $ def
                            { ppCurrent = \c -> "["++ c ++ "]"
                            , ppLayout = const ""
                            , ppOutput = hPutStrLn h }) xmprocs
    -- , workspaces  = myWorkspaces
    } `additionalKeys`
    [ ((myModMask .|. shiftMask, xK_x), spawn "slock")
    , ((myModMask, xK_p),               spawn "dmenu_run -fn 'Montserrat-12:medium:antialias=true'")]
