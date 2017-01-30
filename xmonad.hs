import XMonad
import XMonad.Actions.WorkspaceNames
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.Master
import XMonad.Layout.MessageControl
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)

import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W

import Data.Ratio ((%))
import System.IO
import System.Posix.Unistd

myExtraModMask = mod4Mask
myModMask = mod1Mask

myWorkspaces = ["1:main","2:shell","3:eclipse","4:SideBySide","5","6","7:config","8:break","9:bg"]

defaultLayouts = windowNavigation (
	onWorkspace "1:main"                    ( combine ) $
	onWorkspaces ["2:shell","3:eclipse"]    ( simpleTabbed ||| Full ||| tiled ||| rtiled ) $
	onWorkspace "4:SideBySide"              ( TwoPane 0.03 0.5 ) $
	tiled 		|||  rtiled |||  mtiled        |||
	simpleTabbed	||| Full    |||  combine |||
	rcombine)
  where
    grid = GridRatio (9/10)
    im = withIM (1%7) (Title "Hangouts") grid
    rim = reflectHoriz im
    rtiled = reflectHoriz tiled
    mtiled = Mirror tiled
    tiled   = Tall 1 0.03 0.5
    combine = (grid ||| (simpleTabbed *//* grid )) *||* (ignore NextLayout $ unEscape (simpleTabbed ||| rtiled))
    rcombine = reflectHoriz combine

myLayout = smartBorders . avoidStruts $ defaultLayouts

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
	urgencyHook LibNotifyUrgencyHook w = do
		name     <- getName w
		Just idx <- fmap (W.findTag w) $ gets windowset
		safeSpawn "notify-send" [show name, "workspace " ++ idx]

myManageHook = manageHook gnomeConfig
	<+> composeAll [
		resource =? "synapse" --> doFloat
	,	className =? "Eclipse" --> doShift "3:eclipse"
	,	className =? "jetbrains-idea-ce" --> doShift "5"
	,	className =? "sun-awt-X11-XFramePeer" --> doShift "5"
        ,	stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doF W.swapDown
	,	stringProperty "WM_NAME" =? "thankevan.com/hacking/pomodoro/ - Google Chrome" --> doShift "1:main"
	]
	<+> manageDocks

myXMobarHook xmobar =
    workspaceNamesPP pp {
        ppOutput = hPutStrLn xmobar
        ,ppTitle = xmobarColor "green" "" . shorten 50
    } >>= dynamicLogWithPP 


{-myDualMonitorLogHook :: MonadIO m0 => m0 [Handle] -> m0 [Handle] -> X ()-}
myDualMonitorLogHook xmobar xmobar2 = do
    fadeInactiveLogHook 0.7
    myXMobarHook xmobar2
    myXMobarHook xmobar
    setWMName "LG3D"

mySingleMonitorLogHook xmobar = do
    fadeInactiveLogHook 0.7
    myXMobarHook xmobar
    setWMName "LG3D"

myLogHook = myDualMonitorLogHook

keysToAdd x = [ ((myExtraModMask, xK_n), renameWorkspace defaultXPConfig)
	      , ((myExtraModMask, xK_p), spawn "echo '25 5' > ~/.pomodoro_session")
	      , ((myModMask .|. controlMask, xK_l), spawn "gnome-screensaver-command -l")
--	      , ((myExtraModMask, xK_h), sendKey noModMask xK_Left)
	      --, ((myExtraModMask, xK_j), sendKey noModMask xK_Down)
	      --, ((myExtraModMask, xK_k), sendKey noModMask xK_Up)
	      --, ((myExtraModMask, xK_l), sendKey noModMask xK_Right)
              , ((myModMask, xK_s), spawn "google-chrome http://sponge/lucky")
              --, ((myModMask,                 xK_Right), sendMessage $ Go R)
              --, ((myModMask,                 xK_Left ), sendMessage $ Go L)
              --, ((myModMask,                 xK_Up   ), sendMessage $ Go U)
              --, ((myModMask,                 xK_Down ), sendMessage $ Go D)
              , ((myModMask .|. controlMask, xK_Right), sendMessage $ Swap R)
              , ((myModMask .|. controlMask, xK_Left ), sendMessage $ Swap L)
              , ((myModMask .|. controlMask, xK_Up   ), sendMessage $ Swap U)
              , ((myModMask .|. controlMask, xK_Down ), sendMessage $ Swap D)
	      , ((myModMask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
	      , ((myModMask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
	      , ((myModMask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
	      , ((myModMask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
	      {-, ((myModMask .|. controlMask .|. shiftMask, xK_s    ), sendMessage $ SwapWindow)-}

              , ((myModMask , xK_h ), sendMessage $ escape Expand) -- %! Expand the master area of the sublayout
              , ((myModMask , xK_l ), sendMessage $ escape Shrink) -- %! Shrink the master area of the sublayout
              , ((myModMask , xK_space ), sendMessage $ escape NextLayout) -- %! Expand the master area of the sublayout

	      , ((myExtraModMask, xK_a), sendMessage Taller)
	      , ((myExtraModMask, xK_z), sendMessage Wider)
	      , ((myExtraModMask, xK_r), sendMessage Reset)

	      {-, ((myExtraModMask, xK_Print), spawn "scrot screen_%Y-%m-%d-%H-%M-%S.png -d 1 -e 'mv $f ~/Screenshots/'")-}
	      , ((myExtraModMask , xK_Print), spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -d 1 -u -e 'mv $f ~/Screenshots/'")

	      ]

myMouse x = [ ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]

newMouse x = M.union (mouseBindings defaultConfig x) (M.fromList (myMouse x))

keysToDel x = []

newKeys x = M.union (keys defaultConfig x) (M.fromList(keysToAdd x))

myKeys x = foldr M.delete (newKeys x) (keysToDel x)

myEzKeys = [ ("<XF86AudioRaiseVolume>", spawn "~/.xmonad/scripts/volumeup")
	   , ("<XF86AudioLowerVolume>", spawn "~/.xmonad/scripts/volumedown")
	   , ("<XF86AudioMute>", spawn "~/.xmonad/scripts/volumemute")
           ]


sBar :: String
sBar = "xmobar"
pp = case sBar of
	"xmobar" -> xmobarPP

main = do
    host <- fmap nodeName getSystemID
    xmobar <- spawnPipe "xmobar ~/.xmobarrc"
    xmobar2 <- spawnPipe "xmobar ~/.xmobarrc2"
    xmobar_single <- spawnPipe "xmobar ~/.xmobarrc_single"
    xmonad
    	$ withUrgencyHook LibNotifyUrgencyHook
        $ ewmh defaultConfig {
		layoutHook = myLayout
		,modMask = mod4Mask
		,borderWidth = 1
		,workspaces=myWorkspaces
		,mouseBindings = newMouse
		,keys = myKeys
		,startupHook = do
            setWMName "LG3D"
		,manageHook = myManageHook
        ,logHook = (
            if host == "mattbook"
            then mySingleMonitorLogHook xmobar_single
            else myDualMonitorLogHook xmobar xmobar2
        )
	}`additionalKeysP` myEzKeys


