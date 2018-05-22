import XMonad
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.CopyWindow
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Script
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows (focusUp, focusDown, boringWindows)
import XMonad.Layout.Combo
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
import XMonad.Layout.SubLayouts
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
import XMonad.Util.NamedScratchpad
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare

import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified ColorTheme as Sol
import qualified MyConfig as My

import Control.Monad (when, liftM, sequence)
import Data.List (intercalate, isInfixOf)

import Data.Ratio ((%))
import System.IO

lWinMask = mod4Mask
lAltMask = mod1Mask
rWinMask = mod3Mask
rAltMask = mod2Mask

doSink :: ManageHook
doSink = doF . W.sink =<< ask

myWorkspaces = ["main","term","3","browser","ide","files","7","personal","9"]

defaultLayouts = windowNavigation (
  onWorkspace "main"  ( main ||| mainBsp ||| tbsp ||| tabbed ) $
  onWorkspace "9"  ( main ||| mainBsp ||| tbsp ||| tabbed ) $
  onWorkspace "3"   ( tabbed  ||| tbsp ) $
  onWorkspace "ide"   ( tabbed ||| mtiled ||| tbsp ) $
	tabbed	||| tbsp ||| tiled |||  rtiled ||| mtiled ||| twotiled )
  where
    tabbed = renamed [Replace "tabbed"] $ simpleTabbed
    mos = MosaicAlt M.empty
    rtiled = renamed [Replace "rtiled"] $ reflectHoriz tiled
    mtiled = renamed [Replace "mtiled"] $ Mirror tiled
    tiled  = renamed [Replace "tiled"] $ Tall 1 0.03 0.5
    grid = renamed [Replace "grid"] $ GridRatio (0.5)
    hgrid = GridRatio (0.4)
    bsp = renamed [Replace "bsp"] $ BSP.emptyBSP
    tbsp = subTabbed $ bsp
    twotiled = renamed [Replace "twop"] $ combineTwo (TwoPane 0.03 0.5) (tabbed) (tabbed)
    main = renamed [Replace "main"] $
        combineTwoP (TwoPane 0.03 0.2)
            (combineTwoP (Mirror (TwoPane 0.03 0.2)) (tabbed) (hgrid) (Or (ClassName  "Firefox-esr") (ClassName "Firefox") ) )
            (tabbed )
            (Or (ClassName "Firefox")
                (Or (ClassName "Firefox-esr")
                    (Or (Title "Google Hangouts - goldfarb@google.com")
                        (Title "Google Hangouts - themattgoldfarb@gmail.com"))))
    mainBsp = renamed [Replace "mainBsp"] $
        combineTwoP (TwoPane 0.03 0.2)
            (combineTwoP (Mirror (TwoPane 0.03 0.2)) (tabbed) (hgrid) (Or (ClassName  "Firefox-esr") (ClassName "Firefox") ) )
            (tbsp )
            (Or (ClassName "Firefox")
                (Or (ClassName "Firefox-esr")
                    (Or (Title "Google Hangouts - goldfarb@google.com")
                        (Title "Google Hangouts - themattgoldfarb@gmail.com"))))

myLayout = borderResize $ smartBorders $ avoidStruts $ defaultLayouts

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
	,	className =? "jetbrains-clion" --> doShift "ide" -- move clion to ide
	,	className =? "sun-awt-X11-XFramePeer" --> doShift "ide"
  , fmap ( "https://hangouts.google.com/webchat/iframe3?" `isInfixOf`) (stringProperty "WM_NAME") --> doSink
	,	stringProperty "WM_NAME" =? "Google Hangouts - goldfarb@google.com" --> doShift "main"
	,	stringProperty "WM_NAME" =? "Google Hangouts - themattgoldfarb@gmail.com" --> doShift "main"
	-- ,	stringProperty "WM_NAME" ~? ".* - Cider" --> doShift "ide"
	,	resource =? "google-chrome" --> doFloat
	,	stringProperty "WM_NAME" =? "thankevan.com/hacking/pomodoro/ - Google Chrome" --> doShift "main"
	,	stringProperty "WM_NAME" =? "modal" --> doFloat
	]
	<+> manageDocks
  <+> manageScratchPad
  <+> namedScratchpadManageHook scratchpads

doSwap = do
            name <- liftX (sendMessage  SwapWindow)
            doF W.swapUp




myDynHook = composeAll [
		stringProperty "WM_NAME" =? "Google Hangouts - goldfarb@google.com" -->  doShift "main" <+> doSink
  ,	stringProperty "WM_NAME" =? "chrome-extension://nckgahadagoaajjgafhacjanaoiihapd/mainapp.html?uv_main_window" --> doFloat
  , stringProperty "WM_CLASS" =? "Firefox-esr" --> doShift "main" <+> doSink
  ,	stringProperty "WM_NAME" =? "Google Hangouts - themattgoldfarb@gmail.com" --> doShift "main" <+> doSink 
  , fmap ( "https://hangouts.google.com/webchat/frame3?" `isInfixOf`) (stringProperty "WM_NAME") --> doSink <+> doSwap
  ]

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
    NS "htop" "urxvt -e htop" (title =? "htop") topFloating,
    NS "notes" "gvim --role notes ~/vimwiki/index.md" (role =? "notes") topFloating,
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
        wrapL "[" "]" ( logTitles (xmobarColor Sol.green "") (xmobarColor Sol.base01 ""))]
    , ppSort    = getSortByIndex
    , ppHidden  = const ""
    , ppHiddenNoWindows = const ""
}

unfocusedScreenPP :: PP
unfocusedScreenPP =  focusedScreenPP {
      ppTitle = const ""
    , ppExtras  = [
        wrapL "[" "]" ( logTitles (xmobarColor Sol.green "") (xmobarColor Sol.base01 ""))]
}

mySpace =
    if My.mySuffix == "__laptop" then 60
    else 160

translateWindow :: String -> String
translateWindow name =
    if isInfixOf "Google Hangouts - goldfarb" name then "Hangouts - Work"
    else if isInfixOf "Google Hangouts - themattgoldfarb" name then "Hangouts - Personal"
    else if isInfixOf "- Google Chrome" name then "Chrome - " ++ takeWhile (/= '-') name
    else if isInfixOf "goldfarb@goldfarb" name then "Term -" ++ drop 1 (dropWhile (/= ':') name)
    else name

logTitles ppFocus ppUnfocus =
        let
            windowTitles windowset = sequence (map (fmap showName . getName) (W.index windowset))
                where
                    numWindows = ( length $ W.index windowset)
                    spacing = (quot mySpace numWindows) - 2
                    fw = W.peek windowset
                    showName nw =
                        let
                            window = unName nw
                            name = shorten spacing (translateWindow (show nw))
                        in
                            if maybe False (== window) fw
                                then
                                    ppFocus name
                                else
                                    ppUnfocus name
        in
            withWindowSet $ liftM (Just . (intercalate "][")) . windowTitles

  {-, ((rWinMask, xK_j), spawn "~/.xmonad/scripts/hangoutsmouse.sh down")-}
  {-, ((rWinMask, xK_k), spawn "~/.xmonad/scripts/hangoutsmouse.sh up")-}
  {-, ((rWinMask, xK_l), spawn "~/.xmonad/scripts/hangoutsmouse.sh click")-}
  {-, ((rWinMask, xK_n), spawn "xdotool mousemove 1893 1125 click 1 mousemove restore")-}

myWindowNavKeys x = [
    ((rAltMask,                 xK_l ), sendMessage $ pullGroup R)
  , ((rAltMask,                 xK_h ), sendMessage $ pullGroup L)
  , ((rAltMask,                 xK_j ), sendMessage $ pullGroup D)
  , ((rAltMask,                 xK_k ), sendMessage $ pullGroup U)
  , ((rAltMask,                 xK_m ), withFocused (sendMessage . MergeAll))
  , ((rAltMask,                 xK_u ), withFocused (sendMessage . UnMerge)) 
  , ((rAltMask .|. lWinMask,    xK_k ), onGroup W.focusUp' )
  , ((rAltMask,                 xK_s ), sendMessage $ SwapWindow)
  , ((rAltMask .|. lWinMask,    xK_j ), onGroup W.focusDown' ) ]
  {-, ((rAltMask .|. controlMask, xK_k ), focusUp )-}
  {-, ((rAltMask .|. controlMask, xK_j ), focusDown U) ]-}
myBspKeys x = [
    ((rWinMask,                 xK_l ), sendMessage $ Go R)
  , ((rWinMask,                 xK_h ), sendMessage $ Go L)
  , ((rWinMask,                 xK_j ), sendMessage $ Go D)
  , ((rWinMask,                 xK_k ), sendMessage $ Go U)
  , ((rWinMask .|. lWinMask,    xK_l ), sendMessage $ BSP.ExpandTowards R)
  , ((rWinMask .|. lWinMask,    xK_h ), sendMessage $ BSP.ExpandTowards L)
  , ((rWinMask .|. lWinMask,    xK_j ), sendMessage $ BSP.ExpandTowards D)
  , ((rWinMask .|. lWinMask,    xK_k ), sendMessage $ BSP.ExpandTowards U)
  , ((rWinMask .|. controlMask,   xK_l ), sendMessage $ Swap R)
  , ((rWinMask .|. controlMask,   xK_h ), sendMessage $ Swap L)
  , ((rWinMask .|. controlMask,   xK_j ), sendMessage $ Swap D)
  , ((rWinMask .|. controlMask,   xK_k ), sendMessage $ Swap U)
  , ((rWinMask,                 xK_r ), sendMessage BSP.Rotate)
  , ((rWinMask,                 xK_s ), sendMessage BSP.Swap)
  , ((rWinMask,                 xK_n ), sendMessage BSP.FocusParent)
  , ((rWinMask .|. controlMask, xK_n ), sendMessage BSP.SelectNode)
  , ((rWinMask .|. shiftMask,   xK_n ), sendMessage BSP.MoveNode)
  , ((rWinMask,                 xK_a ), sendMessage BSP.Equalize)
  , ((rWinMask .|. lWinMask,    xK_a ), sendMessage BSP.Balance) 
  , ((rWinMask .|. lAltMask,    xK_l ), sendMessage $ pullGroup R)
  , ((rWinMask .|. lAltMask,    xK_h ), sendMessage $ pullGroup L)
  , ((rWinMask .|. lAltMask,    xK_j ), sendMessage $ pullGroup D)
  , ((rWinMask .|. lAltMask,    xK_k ), sendMessage $ pullGroup U)
  , ((rWinMask,                 xK_m ), withFocused (sendMessage . MergeAll))
  , ((rWinMask,                 xK_u ), withFocused (sendMessage . UnMerge))
  , ((rWinMask .|. shiftMask,    xK_k ), onGroup W.focusUp' )
  , ((rWinMask .|. shiftMask,    xK_j ), onGroup W.focusDown' ) ]
keysToAdd x = [
    ((lWinMask, xK_n), renameWorkspace defaultXPConfig)
  , ((lWinMask .|. shiftMask, xK_s), spawn "$HOME/bin/snipit")
  , ((lAltMask .|. controlMask, xK_l), spawn "~/.xmonad/commands/lockscreen")
  , ((rWinMask, xK_f), spawn "~/.xmonad/commands/lock_mac")
  , ((lAltMask, xK_s), spawn "google-chrome http://sponge/lucky")
  , ((lWinMask, xK_s), scratchpadSpawnActionTerminal "urxvt")
  , ((lWinMask, xK_b), sendMessage ToggleStruts )
  , ((lAltMask , xK_p), spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -d 1 -u -e 'mv $f ~/Screenshots/'") ]
  ++ [ ((lWinMask .|. controlMask .|. shiftMask, k ) , windows $ f i )
        | (i, k) <- zip (workspaces x) [xK_1 ..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask) ] ]
scratchpadKeys x = [
    ((lWinMask .|. controlMask, xK_t), namedScratchpadAction scratchpads "htop")
  , ((lWinMask .|. controlMask, xK_n), namedScratchpadAction scratchpads "notes")
  , ((lWinMask .|. controlMask, xK_m), namedScratchpadAction scratchpads "music")
  , ((lWinMask .|. controlMask, xK_b), namedScratchpadAction scratchpads "bugs")
  , ((lWinMask .|. controlMask, xK_i), namedScratchpadAction scratchpads "inbox")
  , ((lWinMask .|. controlMask, xK_c), namedScratchpadAction scratchpads "calendar") ]
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

myMouse x = [ ((lAltMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
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

myStartupHook = composeAll [
      setWMName "LG3D"
    , dynStatusBarStartup myStatusBar myStatusBarCleanup
    , execScriptHook "start goobuntu-indicator"
    , execScriptHook "start notify-server"
    , execScriptHook "start screensaver"
    , execScriptHook "start trayer"
    , execScriptHook "start xcompmgr"
    , execScriptHook "start xmobarpipes"
    , execScriptHook "start run_google"
    , execScriptHook "start keep_mac_awake"
    , execScriptHook "start drive"
    ]

main = do
    xmonad
      $ withUrgencyHook LibNotifyUrgencyHook
      $ ewmh
      $ docks
      def {
          layoutHook = myLayout
        , modMask = mod4Mask
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#FF0000"
        , borderWidth = 1
        , workspaces=myWorkspaces
        , mouseBindings = myMouseBindings
        , keys = myKeys
        , startupHook = myStartupHook
        , manageHook = myManageHook
        , handleEventHook = myHandleEventHook
        , terminal = myTerminal
        , logHook = myLogHook
      }`additionalKeysP` myAdditionalKeys


