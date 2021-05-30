import XMonad
import System.Exit
import System.IO
-- import XMonad.Hooks.SetWNName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.WorkspaceNames
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal = "xterm"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [
        ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
        , ((modMask, xK_p ), spawn "dmenu_run")
        , ((modMask .|. shiftMask, xK_p ), spawn "gmrun")
        , ((modMask .|. shiftMask, xK_c ), kill)
        , ((modMask .|. controlMask , xK_space ), sendMessage NextLayout)
        , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
        , ((modMask, xK_n ), refresh)
        , ((modMask, xK_Tab ), windows W.focusDown)
        , ((modMask, xK_j ), windows W.focusDown)
        , ((modMask, xK_k ), windows W.focusUp )
        , ((modMask, xK_m ), windows W.focusMaster )
        , ((modMask, xK_Return), windows W.swapMaster)
        , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
        , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )
        , ((modMask, xK_h ), sendMessage Shrink)
        , ((modMask, xK_l ), sendMessage Expand)
        , ((modMask .|. shiftMask, xK_l ), spawn "gnome-screensaver-command --lock")
        , ((modMask, xK_t ), withFocused $ windows . W.sink)
        , ((modMask, xK_comma ), sendMessage (IncMasterN 1))
        , ((modMask , xK_period), sendMessage (IncMasterN (-1)))
        , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
        , ((modMask , xK_q ), restart "xmonad" True)

    ]
    ++

    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


main = do
  xmproc <- spawnPipe "/usr/bin/xmobar"
  xmonad $ docks defaultConfig {
    modMask = mod4Mask, -- command key
    borderWidth = 1,
    normalBorderColor  = "#cccccc",
    focusedBorderColor = "#cd8b00",
    terminal = myTerminal,
    keys = myKeys,
    workspaces = map show [1..8],
    layoutHook=avoidStruts $ layoutHook defaultConfig,
    manageHook=manageHook defaultConfig <+> manageDocks,
    logHook = workspaceNamesPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = id
      }
      >>= dynamicLogWithPP
}
