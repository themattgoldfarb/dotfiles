import XMonad
import XMonad.Actions.WorkspaceNames
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicProperty
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
import XMonad.Util.Font
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.WindowPropertiesRE
import XMonad.Util.WorkspaceCompare

import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified ColorTheme as Sol
import qualified MyConfig as My

import Control.Monad (when, liftM, sequence)
import Data.List (intercalate)

import Data.Ratio ((%))
import System.IO

myExtraModMask = mod4Mask
myModMask = mod1Mask
hyperMask = mod3Mask
mehMask = mod2Mask

doSink :: ManageHook
doSink = doF . W.sink =<< ask

myWorkspaces = ["main","term","3","browser","ide","files","7","personal","9"]

defaultLayouts = windowNavigation (
  onWorkspace "main"   ( main ||| tabbed ) $
  onWorkspace "3"   ( tabbed ) $
  onWorkspace "ide"   ( tabbed ||| mtiled ) $
	tabbed	||| tiled |||  rtiled ||| mtiled ||| bsp )
  where
    tabbed = renamed [Replace "tabbed"] $ simpleTabbed
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

myManageHook = manageHook defaultConfig
	<+> composeAll [
		resource =? "synapse" --> doFloat
	,	className =? "Eclipse" --> doShift "ide" -- move eclipse to ide
	,	className =? "jetbrains-idea-ce" --> doShift "ide" -- move intellij to ide
	,	className =? "sun-awt-X11-XFramePeer" --> doShift "ide"
	,	stringProperty "WM_NAME" =? "Google Hangouts - goldfarb@google.com" --> doShift "main"
	,	stringProperty "WM_NAME" =? "Google Hangouts - themattgoldfarb@gmail.com" --> doShift "main"
	,	stringProperty "WM_NAME" ~? ".* - Cider" --> doShift "ide"
	,	resource =? "google-chrome" --> doFloat
	,	stringProperty "WM_NAME" =? "thankevan.com/hacking/pomodoro/ - Google Chrome" --> doShift "main"
	]
	<+> manageDocks
  <+> manageScratchPad
  <+> namedScratchpadManageHook scratchpads

myDynHook = composeAll [
		stringProperty "WM_NAME" =? "Google Hangouts - goldfarb@google.com" -->  doShift "main" <+> doSink
  ,	stringProperty "WM_NAME" =? "chrome-extension://nckgahadagoaajjgafhacjanaoiihapd/mainapp.html?uv_main_window" --> doFloat
  ,	stringProperty "WM_NAME" =? "Google Hangouts - themattgoldfarb@gmail.com" --> doShift "main" <+> doSink ]

myHandleEventHook = handleEventHook def
  <+> composeAll [
    dynamicPropertyChange "WM_NAME" myDynHook
  , dynStatusBarEventHook myStatusBar myStatusBarCleanup
  ]

topFloating = customFloating (W.RationalRect l t w h)
  where
    h = 0.4
    w = 1
    t = 0
    l = 1-w
bottomFloating = customFloating (W.RationalRect l t w h)
  where
    h = 0.4
    w = 1
    t = 1-h
    l = 1-w
centerFloating = customFloating (W.RationalRect l t w h)
  where
    h = 0.9
    w = 0.9
    t = (1-h)/2
    l = (1-w)/2

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h )
  where
    h = 0.3
    w = 1
    t = 1-h
    l = 1-w

googleMusicCommand = "dex $HOME/.local/share/applications/google-play-music.desktop"
isGoogleMusic = (resource =? "play.google.com__music_listen")
buganizerCommand = "dex $HOME/.local/share/applications/buganizer.desktop"
isBuganizer = (resource =? "b.corp.google.com__savedsearches_432047")
inboxCommand = "dex $HOME/.local/share/applications/inbox.desktop"
isInbox = (resource =? "inbox.google.com__u_0")
calendarCommand = "dex $HOME/.local/share/applications/calendar.desktop"
isCalendar = (resource =? "calendar.google.com__calendar_r")

scratchpads = [
    NS "htop" "urxvt -e htop" (title =? "htop") bottomFloating,
    NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes") topFloating,
    NS "music" googleMusicCommand isGoogleMusic bottomFloating,
    NS "bugs" buganizerCommand isBuganizer centerFloating,
    NS "inbox" inboxCommand isInbox centerFloating,
    NS "calendar" calendarCommand isCalendar centerFloating
    ] where role = stringProperty "WM_WINDOW_ROLE"

myFadeHook = fadeInactiveLogHook fadeAmount
		where fadeAmount = 0.8
myLogHook = myFadeHook
    <+> multiPP focusedScreenPP unfocusedScreenPP

focusedScreenPP :: PP
focusedScreenPP = namedScratchpadFilterOutWorkspacePP $ defaultPP {
      ppLayout  = xmobarColor Sol.yellow ""
    , ppCurrent = xmobarColor Sol.blue ""
    , ppVisible = const ""
    , ppUrgent  = xmobarColor Sol.red ""
    , ppTitle   = const ""
    , ppSep     = " | "
    , ppExtras  = [
        wrapL "         [" "]" ( logTitles (xmobarColor Sol.green "") (xmobarColor Sol.base01 ""))]
    , ppSort    = getSortByIndex
    , ppHidden  = const ""
    , ppHiddenNoWindows = const ""
}

unfocusedScreenPP :: PP
unfocusedScreenPP =  focusedScreenPP { 
      ppTitle = const "" 
    , ppExtras  = [
        wrapL "        [" "]" ( logTitles (xmobarColor Sol.green "") (xmobarColor Sol.base01 ""))]
}

myShorten =
    if My.mySuffix == "__laptop" then 20
    else 30

logTitles ppFocus ppUnfocus =
        let
            windowTitles windowset = sequence (map (fmap showName . getName) (W.index windowset))
                where
                    numWindows = ( length $ W.index windowset)
                    spacing = (quot 160 numWindows) - 2
                    fw = W.peek windowset
                    showName nw =
                        let
                            window = unName nw
                            name = shorten spacing (show nw)
                        in
                            if maybe False (== window) fw
                                then
                                    ppFocus name
                                else
                                    ppUnfocus name
        in
            withWindowSet $ liftM (Just . (intercalate "][")) . windowTitles

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
  , ((myModMask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
  , ((myModMask, xK_s), spawn "google-chrome http://sponge/lucky")
  , ((myExtraModMask, xK_s), scratchpadSpawnActionTerminal "urxvt")
  , ((myExtraModMask, xK_b), sendMessage ToggleStruts )
  , ((myModMask , xK_p), spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -d 1 -u -e 'mv $f ~/Screenshots/'") ]
scratchpadKeys x = [
    ((myExtraModMask .|. controlMask, xK_t), namedScratchpadAction scratchpads "htop")
  , ((myExtraModMask .|. controlMask, xK_n), namedScratchpadAction scratchpads "notes")
  , ((myExtraModMask .|. controlMask, xK_m), namedScratchpadAction scratchpads "music")
  , ((myExtraModMask .|. controlMask, xK_b), namedScratchpadAction scratchpads "bugs")
  , ((myExtraModMask .|. controlMask, xK_i), namedScratchpadAction scratchpads "inbox")
  , ((myExtraModMask .|. controlMask, xK_c), namedScratchpadAction scratchpads "calendar") ]
keysToDel x = []
newKeys x = M.unions [ (keys defaultConfig x)
                     , (M.fromList(keysToAdd x))
                     , (M.fromList(myWindowNavKeys x))
                     , (M.fromList(myBspKeys x))
                     , (M.fromList(scratchpadKeys x)) ]
myKeys x = foldr M.delete (newKeys x) (keysToDel x)

myAdditionalKeys = [
    ("<XF86AudioRaiseVolume>", spawn "~/.xmonad/scripts/volumeup")
  , ("<XF86AudioLowerVolume>", spawn "~/.xmonad/scripts/volumedown")
  , ("<XF86AudioMute>", spawn "~/.xmonad/scripts/volumemute")
  , ("<XF86MonBrightnessUp>", spawn "~/.xmonad/scripts/brightness.sh up")
  , ("<XF86MonBrightnessDown>", spawn "~/.xmonad/scripts/brightness.sh down") ]

myMouse x = [ ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
myMouseBindings x = M.union (mouseBindings defaultConfig x) (M.fromList (myMouse x))

myTerminal = "urxvt"

sBar :: String
sBar = "xmobar"
pp = case sBar of
	"xmobar" -> xmobarPP

myXmobarMasterConfig = "~/.xmonad/xmobarmaster" ++ My.mySuffix
myXmobarSlaveConfig = "~/.xmonad/xmobarslave" ++ My.mySuffix

myStatusBar :: ScreenId -> IO Handle
myStatusBar (S 0) = spawnPipe $ "xmobar -x 0 " ++ myXmobarMasterConfig
myStatusBar (S s) = spawnPipe $ "xmobar -x " ++ show s ++ " " ++ myXmobarSlaveConfig

myStatusBarCleanup :: IO ()
myStatusBarCleanup = return ()

myStartupHook = 
    setWMName "LG3D"
    <+> dynStatusBarStartup myStatusBar myStatusBarCleanup

main = do
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
        , startupHook = myStartupHook
        , manageHook = myManageHook
        , handleEventHook = myHandleEventHook
        , terminal = myTerminal
        , logHook = myLogHook 
      }`additionalKeysP` myAdditionalKeys


