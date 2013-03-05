import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))

myModMask = mod4Mask

myWorkspaces = [
      "3:Mail",  "4:Misc",
      "1:Term",  "2:Web"
   ]

numKeys = [
      xK_3, xK_4, 
      xK_1, xK_2 
   ]

myKeyBindings = [
      ((myModMask, xK_b), sendMessage ToggleStruts),
      ((myModMask, xK_a), sendMessage MirrorShrink),
      ((myModMask, xK_z), sendMessage MirrorExpand),
      ((myModMask, xK_p), spawn "synapse"),
      ((myModMask, xK_u), focusUrgent)
   ]

myKeys = myKeyBindings ++ [
      ((m .|. myModMask, k), windows $ f i)
      | (i, k) <- zip myWorkspaces numKeys,
      (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
   ] ++
   M.toList (planeKeys myModMask (Lines 4) Finite) ++
   [
      ((m .|. myModMask, key), screenWorkspace sc
      >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2],
      (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
   ]

myManagementHooks :: [ManageHook]
myManagementHooks = [
      resource =? "synapse" --> doIgnore,
      resource =? "stalonetray" --> doIgnore,
      (className =? "Pidgin") --> doF (W.shift "3:Mail")
   ]

main = do
   xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
   xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
      modMask    = myModMask,
      focusedBorderColor = "#ff0000",
      normalBorderColor  = "#cccccc",
      terminal   = "terminator",
      borderWidth= 1,
      workspaces = myWorkspaces,
      layoutHook = smartBorders(
         avoidStruts(ResizableTall 1 (3/100) (1/2) []
            ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
            ||| noBorders Full
            ||| Grid
            ||| ThreeColMid 1 (3/100) (3/4)
            ||| Circle
         )
      ),
      startupHook  = do
         setWMName "LG3D" 
         windows $ W.greedyView "1:Term"
         spawn "~/.xmonad/startup-hook",
      manageHook   = manageHook
         defaultConfig <+> composeAll
         myManagementHooks <+> manageDocks,
      handleEventHook = fullscreenEventHook,
      logHook      = dynamicLogWithPP $ xmobarPP {
         ppOutput  = hPutStrLn xmproc,
         ppTitle   = xmobarColor "#eeeeee" "" . shorten 80,
         ppCurrent = xmobarColor "#e6744c" "" . wrap "[" "]",
         ppVisible = xmobarColor "#c185a7" "" . wrap "(" ")",
         ppUrgent  = xmobarColor "#cc0000" "" . wrap "{" "}"
      }
   }
    `additionalKeys` myKeys
