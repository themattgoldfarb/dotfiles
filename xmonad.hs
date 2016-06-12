import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Hooks.ManageDocks
--import XMonad.Layout.GridVariants
import XMonad.Layout.IM
import Data.Ratio ((%))
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.WorkspaceNames
import XMonad.Prompt
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.PerWorkspace
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Hooks.SetWMName

myExtraModMask = mod4Mask
myModMask = mod1Mask

myWorkspaces = ["1:main","2:shell","3:eclipse","4:SideBySide","5","6","7:config","8:break","9:bg"]

defaultLayouts = windowNavigation (
	onWorkspace "1:main"                    ( combine ||| rtiled ) $
	onWorkspaces ["2:shell","3:eclipse"]    ( simpleTabbed ||| Full ||| tiled ||| rtiled ) $
	onWorkspace "4:SideBySide"              ( TwoPane 0.03 0.5 ) $
	tiled 		|||  rtiled |||  mtiled        |||
	simpleTabbed	||| Full    |||  combine |||
	rcombine)
  where
 --   split   = SplitGrid XMonad.Layout.GridVariants.L 1 1 (1/2) (3/3) (5/100)
  --  rsplit = reflectHoriz split
    grid = GridRatio (9/10)
    im = withIM (1%7) (Title "Hangouts") grid
    rim = reflectHoriz im
    rtiled = reflectHoriz tiled
    mtiled = Mirror tiled
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    combine = combineTwo (TwoPane 0.03 0.2) (GridRatio 0.9) (simpleTabbed)
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
	,	className =? "sun-awt-X11-XFramePeer" --> doShift "5"
        ,	stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doF W.swapDown
	,	stringProperty "WM_NAME" =? "thankevan.com/hacking/pomodoro/ - Google Chrome" --> doShift "1:main"
	]
	<+> manageDocks


myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
		where fadeAmount = 0.7

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
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    xmobar2 <- spawnPipe "xmobar ~/.xmobarrc2"
    xmonad 
    	$ withUrgencyHook LibNotifyUrgencyHook
        $ ewmh defaultConfig {
		layoutHook = myLayout
		,modMask = mod4Mask
		,borderWidth = 0
		,workspaces=myWorkspaces
		,mouseBindings = newMouse
		,keys = myKeys
		,startupHook = setWMName "LG3D"
	--	,startupHook = do
	--		spawn "xcompmgr -n"
		,manageHook = myManageHook
		--,handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
		,logHook =
		    myLogHook <+>
		    ( workspaceNamesPP pp
			{ ppOutput = hPutStrLn xmobar2
			, ppTitle = xmobarColor "green" "" . shorten 50
		    } >>= dynamicLogWithPP ) <+>
		    ( workspaceNamesPP pp
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "green" "" . shorten 50
		    } >>= dynamicLogWithPP ) <+>
		    ( setWMName "LG3D" )
		   -- 	{ ppOutput = hPutStrLn xmobar2
		   --     , ppTitle = xmobarColor "green" "" .shorten 50
		   --     } >>= dynamicLogWithPP
		     -- <+> myLogHook
	}`additionalKeysP` myEzKeys


