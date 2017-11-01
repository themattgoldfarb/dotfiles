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
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
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

myExtraModMask = mod4Mask
myModMask = mod1Mask

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

defaultLayouts = windowNavigation (
  onWorkspace "1"   ( combine ||| rcombine ) $
  onWorkspace "9"   ( combine ||| rcombine ) $
  onWorkspace "2"   ( tabbed ) $
  onWorkspace "3"   ( tabbed ) $
  onWorkspace "5"   ( tabbed ||| mtiled ) $
	tabbed	||| tiled |||  rtiled ||| mtiled)
  where
    tabbed = renamed [Replace "tabbed"] $ simpleTabbed
    mos = MosaicAlt M.empty
    rtiled = renamed [Replace "rtiled"] $ reflectHoriz tiled
    mtiled = renamed [Replace "mtiled"] $ Mirror tiled
    tiled  = renamed [Replace "tiled"] $ Tall 1 0.03 0.5
    grid = renamed [Replace "grid"] $ GridRatio (0.5)
    combine = renamed [Replace "combine"] $ (grid ||| (simpleTabbed *//**** grid )) *||**** (ignore NextLayout $ unEscape (simpleTabbed ||| rtiled))
    rcombine = renamed [Replace "rcombine"] $ reflectHoriz combine

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
	,	className =? "Eclipse" --> doShift "3"
	,	className =? "jetbrains-idea-ce" --> doShift "5"
	,	className =? "sun-awt-X11-XFramePeer" --> doShift "5"
        ,	stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doF W.swapDown
	,	stringProperty "WM_NAME" =? "thankevan.com/hacking/pomodoro/ - Google Chrome" --> doShift "1"
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

	      , ((myModMask .|. controlMask .|. shiftMask, xK_l), sendMessage $ Move R)
	      , ((myModMask .|. controlMask .|. shiftMask, xK_h), sendMessage $ Move L)
	      , ((myModMask .|. controlMask .|. shiftMask, xK_k), sendMessage $ Move U)
	      , ((myModMask .|. controlMask .|. shiftMask, xK_j), sendMessage $ Move D)
	      {-, ((myModMask .|. controlMask .|. shiftMask, xK_s    ), sendMessage $ SwapWindow)-}

              , ((myModMask , xK_h ), sendMessage $ escape Expand) -- %! Expand the master area of the sublayout
              , ((myModMask , xK_l ), sendMessage $ escape Shrink) -- %! Shrink the master area of the sublayout
              , ((myModMask , xK_space ), sendMessage $ escape NextLayout) -- %! Expand the master area of the sublayout

				{-, ((myExtraModMask, xK_a), sendMessage Taller)-}
				{-, ((myExtraModMask, xK_z), sendMessage Wider)-}
				{-, ((myExtraModMask, xK_r), sendMessage Reset)-}

     , ((myExtraModMask .|. shiftMask  , xK_a    ), withFocused (sendMessage . expandWindowAlt))
     , ((myExtraModMask .|. shiftMask  , xK_z    ), withFocused (sendMessage . shrinkWindowAlt))
     , ((myExtraModMask .|. shiftMask  , xK_s    ), withFocused (sendMessage . tallWindowAlt))
     , ((myExtraModMask .|. shiftMask  , xK_d    ), withFocused (sendMessage . wideWindowAlt))
     , ((myExtraModMask .|. shiftMask, xK_r), sendMessage resetAlt)

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
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    xmonad
    	$ withUrgencyHook LibNotifyUrgencyHook
        $ ewmh defaultConfig {
		layoutHook = myLayout
		,modMask = mod4Mask
		,borderWidth = 1
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
				{-( workspaceNamesPP pp-}
          {-{ ppOutput = hPutStrLn xmproc-}
          {-{-, ppLayout = shorten 50-}-}
          {-, ppTitle = xmobarColor "green" "" . shorten 50-}
            {-} >>= dynamicLogWithPP ) <+>-}
				{-( setWMName "LG3D" )-}
		   -- 	{ ppOutput = hPutStrLn xmobar2
		   --     , ppTitle = xmobarColor "green" "" .shorten 50
		   --     } >>= dynamicLogWithPP
		     -- <+> myLogHook
	}`additionalKeysP` myEzKeys


