import XMonad
import XMonad.Actions.WorkspaceNames
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.ComboP
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
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WindowPropertiesRE

import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W

import Data.Ratio ((%))
import System.IO

myExtraModMask = mod4Mask
myModMask = mod1Mask
hyperMask = mod3Mask
mehMask = mod2Mask

myWorkspaces = ["main","term","3","browser","ide","files","7","personal","9"]

defaultLayouts = windowNavigation (
  onWorkspace "main"   ( main ||| tabbed ) $
  onWorkspace "term"   ( tabbed ) $
  onWorkspace "3"   ( tabbed ) $
  onWorkspace "ide"   ( tabbed ||| mtiled ) $
	tabbed	||| tiled |||  rtiled ||| mtiled ||| bsp )
  where
    tabbed = renamed [Replace "tabbed"] $ simpleTabbed
    tabbed2345 = renamed [Replace "tabbed"] $ simpleTabbed
    mos = MosaicAlt M.empty
    rtiled = renamed [Replace "rtiled"] $ reflectHoriz tiled
    mtiled = renamed [Replace "mtiled"] $ Mirror tiled
    tiled  = renamed [Replace "tiled"] $ Tall 1 0.03 0.5
    grid = renamed [Replace "grid"] $ GridRatio (0.5)
    bsp = renamed [Replace "bsp"] $ BSP.emptyBSP
    main = renamed [Replace "main"] $
        combineTwoP (TwoPane 0.03 0.2)
            (combineTwoP (Mirror (TwoPane 0.03 0.2)) (tabbed) (grid) (ClassName  "Firefox") )
            (tabbed )
            (Or (ClassName "Firefox") (Or (Title "Google Hangouts - goldfarb@google.com") (Title "Google Hangouts - themattgoldfarb@gmail.com")))

myLayout = smartBorders $ avoidStruts $ defaultLayouts

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
	urgencyHook LibNotifyUrgencyHook w = do
		name     <- getName w
		Just idx <- fmap (W.findTag w) $ gets windowset
		safeSpawn "notify-send" [show name, "workspace " ++ idx]

myManageHook = manageHook gnomeConfig
	<+> composeAll [
		resource =? "synapse" --> doFloat
	,	className =? "Eclipse" --> doShift "ide" -- move eclipse to ide
	,	className =? "jetbrains-idea-ce" --> doShift "ide" -- move intellij to ide
	,	className =? "sun-awt-X11-XFramePeer" --> doShift "ide"
	,	stringProperty "WM_NAME" =? "Google Hangouts - goldfarb@google.com" --> doShift "main"
	,	stringProperty "WM_NAME" =? "Google Hangouts - themattgoldfarb@gmail.com" --> doShift "main"
	,	stringProperty "WM_NAME" ~? ".* - Cider" --> doShift "ide"
  ,	stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doF W.swapDown
	,	resource =? "google-chrome" --> doFloat
	,	stringProperty "WM_NAME" =? "thankevan.com/hacking/pomodoro/ - Google Chrome" --> doShift "main"
	]
	<+> manageDocks


myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
		where fadeAmount = 0.8

myWindowNavKeys x = [
    ((mehMask,                 xK_l ), sendMessage $ Go R)
  , ((mehMask,                 xK_h ), sendMessage $ Go L)
  , ((mehMask,                 xK_j ), sendMessage $ Go D)
  , ((mehMask,                 xK_k ), sendMessage $ Go U)
  , ((mehMask .|. shiftMask,   xK_l ), sendMessage $ Swap R)
  , ((mehMask .|. shiftMask,   xK_h ), sendMessage $ Swap L)
  , ((mehMask .|. shiftMask,   xK_j ), sendMessage $ Swap D)
  , ((mehMask .|. shiftMask,   xK_k ), sendMessage $ Swap U)
  , ((mehMask .|. controlMask, xK_l ), sendMessage $ Move R)
  , ((mehMask .|. controlMask, xK_h ), sendMessage $ Move L)
  , ((mehMask .|. controlMask, xK_j ), sendMessage $ Move D)
  , ((mehMask .|. controlMask, xK_k ), sendMessage $ Move U) ]
myBspKeys x = [
    ((hyperMask,                  xK_l ), sendMessage $ BSP.ExpandTowards R)
  , ((hyperMask,                  xK_h ), sendMessage $ BSP.ExpandTowards L)
  , ((hyperMask,                  xK_j ), sendMessage $ BSP.ExpandTowards D)
  , ((hyperMask,                  xK_k ), sendMessage $ BSP.ExpandTowards U)
  , ((hyperMask .|. shiftMask,    xK_l ), sendMessage $ BSP.ShrinkFrom R)
  , ((hyperMask .|. shiftMask,    xK_h ), sendMessage $ BSP.ShrinkFrom L)
  , ((hyperMask .|. shiftMask,    xK_j ), sendMessage $ BSP.ShrinkFrom D)
  , ((hyperMask .|. shiftMask,    xK_k ), sendMessage $ BSP.ShrinkFrom U)
  , ((hyperMask,                  xK_r ), sendMessage BSP.Rotate)
  , ((hyperMask,                  xK_s ), sendMessage BSP.Swap)
  , ((hyperMask,                  xK_n ), sendMessage BSP.FocusParent)
  , ((hyperMask .|. shiftMask,    xK_n ), sendMessage BSP.SelectNode)
  , ((hyperMask .|. controlMask,  xK_n ), sendMessage BSP.MoveNode)
  , ((hyperMask,                  xK_a ), sendMessage BSP.Balance)
  , ((hyperMask,                  xK_f ), sendMessage BSP.Equalize)
  , ((hyperMask,                  xK_e ), sendMessage BSP.RotateL)
  , ((hyperMask,                  xK_t ), sendMessage BSP.RotateR) ]
keysToAdd x = [
    ((myExtraModMask, xK_n), renameWorkspace defaultXPConfig)
  , ((myModMask .|. controlMask, xK_l), spawn "gnome-screensaver-command -l")
  , ((myModMask, xK_s), spawn "google-chrome http://sponge/lucky")
  , ((myModMask , xK_p), spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -d 1 -u -e 'mv $f ~/Screenshots/'") ]
keysToDel x = []
newKeys x = M.unions [ (keys defaultConfig x)
                     , (M.fromList(keysToAdd x))
                     , (M.fromList(myWindowNavKeys x))
                     , (M.fromList(myBspKeys x)) ]
myKeys x = foldr M.delete (newKeys x) (keysToDel x)

myAdditionalKeys = [
    ("<XF86AudioRaiseVolume>", spawn "~/.xmonad/scripts/volumeup")
  , ("<XF86AudioLowerVolume>", spawn "~/.xmonad/scripts/volumedown")
  , ("<XF86AudioMute>", spawn "~/.xmonad/scripts/volumemute") ]

myMouse x = [ ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
myMouseBindings x = M.union (mouseBindings defaultConfig x) (M.fromList (myMouse x))


sBar :: String
sBar = "xmobar"
pp = case sBar of
	"xmobar" -> xmobarPP

main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    xmobar2 <- spawnPipe "xmobar ~/.xmobarrc2"
    xmonad
      $ withUrgencyHook LibNotifyUrgencyHook
      $ ewmh
      $ docks
      def {
          layoutHook = myLayout
        , modMask = mod4Mask
        , normalBorderColor = "#000000"
        , borderWidth = 0
        , workspaces=myWorkspaces
        , mouseBindings = myMouseBindings
        , keys = myKeys
        , startupHook = setWMName "LG3D"
        , manageHook = myManageHook
        , logHook = myLogHook <+> (
            workspaceNamesPP pp
            { ppOutput = hPutStrLn xmobar2
            , ppTitle = xmobarColor "green" "" . shorten 50
            } >>= dynamicLogWithPP )
      }`additionalKeysP` myAdditionalKeys


