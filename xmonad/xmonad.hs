import XMonad
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.Commands
import XMonad.Actions.CopyWindow
import XMonad.Actions.TagWindows
import XMonad.Config
import XMonad.Config.Gnome
import XMonad.Config.Mate
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Script
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.XPropManage
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows (focusUp, focusDown, boringWindows)
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.Grid
import qualified XMonad.Layout.GridVariants as GV
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Master
import XMonad.Layout.MessageControl
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Prompt.Window ( windowPromptGoto )
import XMonad.Prompt.XMonad
import XMonad.Util.Font
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare
import XMonad.Prelude (fromMaybe)

import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified ColorTheme as Sol

import Control.Monad (when, liftM, sequence)
import Data.List (intercalate, isInfixOf)

import Data.Ratio ((%))
import System.IO

lWinMask = mod4Mask
lAltMask = mod1Mask
rWinMask = mod3Mask
rAltMask = mod2Mask

-- doSink :: ManageHook
-- doSink = doF . W.sink =<< ask

myFirstWorkspaces = [
    (xK_1, "main"), (xK_2, "term"), (xK_3, "pterm")
  , (xK_4, "browser"), (xK_5, "ide"), (xK_6, "files")
  , (xK_7, "other"), (xK_8, "personal"), (xK_9, "pother") ]
mySecondWorkspaces = [
    (xK_1, "main2"), (xK_2, "term2"), (xK_3, "32")
  , (xK_4, "browser2"), (xK_5, "ide2"), (xK_6, "files2")
  , (xK_7, "72"), (xK_8, "personal2"), (xK_9, "92") ]
myThirdWorkspaces = [
    (xK_1, "main3"), (xK_2, "term3"), (xK_3, "33")
  , (xK_4, "browser3"), (xK_5, "ide3"), (xK_6, "files3")
  , (xK_7, "73"), (xK_8, "personal3"), (xK_9, "93") ]

myWorkspaces = [] ++ (map snd myFirstWorkspaces) ++ (map snd mySecondWorkspaces) ++ (map snd myThirdWorkspaces)



 -- onWorkspace "main2"  ( main ||| mainBsp ||| tbsp ||| tabbed ||| sgrid ) $

defaultLayouts = ( windowNavigation (
  onWorkspaces ["main", "main2"] (tbsp) $
  onWorkspaces ["term", "pterm"] (tiled ||| tabbed) $
  onWorkspaces ["ide"] (tiled ||| tabbed) $
  tiled ||| rtiled ||| mtiled ||| tabbed ||| tbsp ||| sgrid))
  where
    tabbed = renamed [Replace "tabbed"] $ simpleTabbed
    mos = MosaicAlt M.empty
    rtiled = renamed [Replace "rtiled"] $ reflectHoriz tiled
    spaced = renamed [Replace "spaced"] $ spacingRaw True (Border 20 20 20 20) True (Border 20 20 20 20) True $ grid
    mtiled = renamed [Replace "mtiled"] $ Mirror tiled
    tiled  = renamed [Replace "tiled"] $ Tall 1 0.03 0.5
    grid = renamed [Replace "grid"] $ GridRatio (0.5)
    hgrid = GridRatio (0.4)
    bsp = renamed [Replace "bsp"] $ BSP.emptyBSP
    tbsp = renamed [Replace "tbsp"] $ subTabbed $ bsp
    sgrid = GV.SplitGrid GV.T 2 1 (2/3) (16/10) (5/100)
    twotiled = renamed [Replace "twop"] $ combineTwo (TwoPane 0.03 0.5) (tabbed) (tabbed)
    left = renamed [Replace "left"] $
        combineTwoP (Mirror $ reflectHoriz $ TwoPane 0.03 0.45)
            (tabbed)
            (combineTwoP (Mirror $ reflectHoriz (TwoPane 0.03 0.3))
                (combineTwoP (TwoPane 0.03 0.5) (TwoPane 0.03 0.5) (tabbed) (Tagged "hangouts"))
                (tabbed)
                (Or (Tagged "pomodoro") (Or (ClassName "Firefox-esr") (Tagged "hangouts") )))
            (Or (Tagged "dynamite") (Or (Tagged "vimwiki") (Or (Tagged "memegen") (Or (Tagged "inbox") (Or (Tagged "gnosis") (Tagged "gmail"))))))
    main = renamed [Replace "main"] $
        combineTwoP (TwoPane 0.03 0.2)
            (combineTwoP (Mirror (TwoPane 0.03 0.2)) (tabbed) (hgrid) (Or (ClassName  "Firefox-esr") (ClassName "Firefox") ) )
            (tabbed )
            (Or (ClassName "Firefox")
                (Or (ClassName "Firefox-esr")
                    (Or (ClassName "gnosis.googleplex.com")
                        (Or (Title "Inbox - goldfarb@google.com")
                            (Title "Google Hangouts - themattgoldfarb@gmail.com")))))
    mainBsp = renamed [Replace "mainBsp"] $
        combineTwoP (TwoPane 0.03 0.2) (combineTwoP (Mirror (TwoPane 0.03 0.2)) (tabbed) (hgrid) (Or (ClassName  "Firefox-esr") (ClassName "Firefox") ) )
            (tbsp )
            (Or (ClassName "FirEfox")
                (Or (ClassName "Firefox-esr")
                    (Or (ClassName "gnosis.googleplex.com")
                        (Or (Title "Google Hangouts - goldfarb@google.com")
                            (Title "Google Hangouts - themattgoldfarb@gmail.com")))))

myLayout = avoidStruts $ borderResize $ defaultLayouts

-- Get the name of the active layout.
getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
    workspaces <- gets windowset
    return $ description . W.layout . W.workspace . W.current $ workspaces

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

xPropMatches = [ ([ (wM_CLASS, any ("inbox" `isInfixOf`))], (\w -> return (W.shift "main2") ))
               , ([ (wM_CLASS, any ("inbox" `isInfixOf`))], pmX (addTag "inbox" ))
               , ([ (wM_CLASS, any ("memegen" `isInfixOf`))], pmX (addTag "memegen" ))
               , ([ (wM_CLASS, any ("memegen" `isInfixOf`))], (\w -> return (W.shift "main2") ))
               , ([ (wM_CLASS, any ("chat" `isInfixOf`))], pmX (addTag "dynamite" ))
               , ([ (wM_CLASS, any ("chat" `isInfixOf`))], (\w -> return (W.shift "main2") ))
               , ([ (wM_CLASS, any ("localhost" `isInfixOf`))], pmX (addTag "vimwiki" ))
               , ([ (wM_CLASS, any ("gnosis" `isInfixOf`))], pmX (addTag "gnosis" ))
               , ([ (wM_CLASS, any ("localhost" `isInfixOf`))], (\w -> return (W.shift "main2") ))
               , ([ (wM_CLASS, any ("mail.google" `isInfixOf`))], (\w -> return (W.shift "main2") ))
               , ([ (wM_CLASS, any ("mail.google" `isInfixOf`))], pmX (addTag "gmail" ))
               , ([ (wM_CLASS, any ("crx_nckgah" `isInfixOf`))], (\w -> return (W.shift "main2") ))
               , ([ (wM_CLASS, any ("crx_nckgah" `isInfixOf`))], pmX (addTag "hangouts" ))
               , ([ (wM_CLASS, any ("pomodoro" `isInfixOf`))], (\w -> return (W.shift "main2") ))
               , ([ (wM_CLASS, any ("pomodoro" `isInfixOf`))], pmX (addTag "pomodoro" ))
               ]

myManageHook = manageHook def
  <+> composeAll [
    resource =? "synapse" --> doFloat
  ,  className =? "XTerm" --> doFloat
  ,  appName =? "xmessage" --> doFloat
  , appName =? "xclock" --> doRectFloat (W.RationalRect (1%4) (1%4) (1%2) (1%2))
  , className =? "Eclipse" --> doShift "ide" -- move eclipse to ide
  , className =? "jetbrains-idea-ce" --> doShift "ide" -- move intellij to ide
  , className =? "jetbrains-clion" --> doShift "ide" -- move clion to ide
  , className =? "sun-awt-X11-XFramePeer" --> doShift "ide"
  , propertyToQuery (Role "GtkFileChooserDialog") --> doRectFloat (W.RationalRect (1%4) (1%4) (1%2) (1%2))
  , fmap ( "https://hangouts.google.com/webchat/iframe3?" `isInfixOf`) (stringProperty "WM_NAME") --> doSink
  , stringProperty "WM_NAME" =? "Google Hangouts - goldfarb@google.com" --> doShift "main2"
  , stringProperty "WM_NAME" =? "Inbox - goldfarb@google.com" --> doShift "main2"
  , stringProperty "WM_NAME" =? "Google Hangouts - themattgoldfarb@gmail.com" --> doShift "main2"
  -- , stringProperty "WM_NAME" ~? ".* - Cider" --> doShift "ide"
  , resource =? "google-chrome" --> doSink
  , stringProperty "WM_NAME" =? "thankevan.com/hacking/pomodoro/ - Google Chrome" --> doShift "main"
  , stringProperty "WM_NAME" =? "modal" --> doFloat
  ]
  <+> manageDocks
  <+> namedScratchpadManageHook scratchpads
  <+> xPropManageHook xPropMatches

doSwap = do
            name <- liftX (sendMessage  SwapWindow)
            doF W.swapUp




myDynHook = composeAll [
    stringProperty "WM_NAME" =? "Google Hangouts - goldfarb@google.com" -->  doShift "main2" <+> doSink
  , stringProperty "WM_NAME" =? "Inbox - goldfarb@google.com" -->  doShift "main2" <+> doSink
  , stringProperty "WM_NAME" =? "chrome-extension://nckgahadagoaajjgafhacjanaoiihapd/mainapp.html?uv_main_window" --> doFloat
  , stringProperty "WM_CLASS" =? "XTerm" --> doFloat
  , stringProperty "WM_CLASS" =? "Firefox-esr" --> doShift "main" <+> doSink
  , stringProperty "WM_NAME" =? "Google Hangouts - themattgoldfarb@gmail.com" --> doShift "main" <+> doSink
  , fmap ( "https://hangouts.google.com/webchat/frame3?" `isInfixOf`) (stringProperty "WM_NAME") --> doSink <+> doSwap
  ]

myHandleEventHook = handleEventHook def
  <+> composeAll [
    dynamicPropertyChange "WM_NAME" myDynHook
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

youtubeMusicCommand = "dex $HOME/.local/share/applications/youtube-music.desktop"
isYoutubeMusic = (resource =? "music.youtube.com")
buganizerCommand = "dex $HOME/.local/share/applications/buganizer.desktop"
isBuganizer = (resource =? "b.corp.google.com__savedsearches_432047")
gmailCommand = "dex $HOME/.local/share/applications/gmail.desktop"
isGmail = (resource =? "go__direct-to-me-goldfarb-google.com-google.com-mail")
hotkeysCommand = "dex $HOME/.local/share/applications/hotkeys.desktop"
isHotkeys = (className =? "hotkeyref")
chatCommand = "dex $HOME/.local/share/applications/chat.desktop"
isChat = (resource =? "go__goldfarb-dynamite")
calendarCommand = "dex $HOME/.local/share/applications/calendar.desktop"
isCalendar = (resource =? "calendar.google.com__calendar_r")

scratchpads = [
    NS "htop" "urxvt -e htop" (title =? "htop") topFloating,
    NS "hotkeys" hotkeysCommand isHotkeys centerFloating,
    NS "notes" "gnome-terminal --role notes --hide-menubar --profile=scratchpad -- vim ~/vimwiki/index.md" (role =? "notes") topFloating,
    NS "music" youtubeMusicCommand isYoutubeMusic bottomFloating,
    NS "bugs" buganizerCommand isBuganizer centerFloating,
    NS "inbox" gmailCommand isGmail centerFloating,
    NS "chat" chatCommand isChat centerFloating,
    NS "calendar" calendarCommand isCalendar centerFloating
    ] where role = stringProperty "WM_WINDOW_ROLE"

myFadeHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8
myLogHook = myFadeHook

formatFocused   = wrap "[" "]" . xmobarColor "#ff79c6" "" . shorten 30 . xmobarStrip
formatUnfocused = wrap "(" ")" . xmobarColor "#bd93f9" "" . shorten 30 . xmobarStrip

focusedScreenPP :: ScreenId -> PP
focusedScreenPP s = filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP {
      ppLayout  = const "" --xmobarColor Sol.yellow ""
    , ppCurrent = const "" --xmobarColor Sol.blue ""
    , ppVisible = const ""
    , ppUrgent  = const "" --xmobarColor Sol.red ""
    , ppTitle   = const ""
    , ppSep     = " "
    , ppExtras  = [
        xmobarColorL "#33FFFF" "" $ logCurrentOnScreen s 
      , xmobarColorL "#FFFF33" "" $ logLayoutOnScreen s
      , logTitlesOnScreen s formatFocused formatUnfocused .| logConst "With great power comes great responsibility..."
    ]
    , ppSort    = getSortByIndex
    , ppHidden  = const ""
    , ppHiddenNoWindows = const ""
}

mySpace = 320

translateWindow :: String -> String
translateWindow name =
    if isInfixOf "Google Hangouts - goldfarb" name then "\xf651"
    else if isInfixOf "Google Hangouts - themattgoldfarb" name then "\xf075"
    else if isInfixOf "Google Hangouts" name then "\xf075"
    else if isInfixOf "- Google.com Mail" name then " \xf0e0 "
    else if isInfixOf "Google.com - Calendar" name then " \xf073 "
    else if isInfixOf "- Google Chrome" name then " \xf268" ++ take ((length name) - 15) name
    else if isInfixOf "goldfarb@goldfarb" name then "\xf120" ++ drop 1 (dropWhile (/= ':') name)
    else name

logTitles2 ppFocus ppUnfocus =
        let
            windowTitles windowset = sequence (map (fmap showName . getName) (W.index windowset))
                where
                    numWindows = ( length $ W.index windowset)
                    spacing =
                        if (quot mySpace numWindows) - 2 > 30 then 30
                        else (quot mySpace numWindows)
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



mySecondWorkspaceKeys x = [
    ((rWinMask,                 key  ), (windows $ W.greedyView ws))
    | (key,ws) <- mySecondWorkspaces
 ] ++ [
    ((rWinMask .|. shiftMask,   key  ), (windows $ W.shift ws))
    | (key,ws) <- mySecondWorkspaces
 ]

myThirdWorkspaceKeys x = [
    ((rAltMask,                 key  ), (windows $ W.greedyView ws))
    | (key,ws) <- myThirdWorkspaces
 ] ++ [
    ((rAltMask .|. shiftMask,   key  ), (windows $ W.shift ws))
    | (key,ws) <- myThirdWorkspaces
 ]

myGroupdWorkspaceKeys x = [
    ((rAltMask,                 key  ), (windows $ W.greedyView ws))
    | (key,ws) <- myThirdWorkspaces
 ] ++ [
    ((rAltMask .|. shiftMask,   key  ), (windows $ W.shift ws))
    | (key,ws) <- myThirdWorkspaces
 ]



myScreenNavKeys x = [
    ((m .|. lWinMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r, xK_f] [2, 0, 1, 3]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- perLayoutKey :: ((KeyMask, KeySym), X (), X ()) -> ((KeyMask, KeySym), X())
perLayoutKey (keys, tbspFunc, normalFunc) =
    (( keys) , do
    layout <- getActiveLayoutDescription
    case layout of
        "tbsp" -> tbspFunc
        _      -> normalFunc
    )

myPerLayoutKeys x = map perLayoutKey [
    ((lWinMask, xK_h ), sendMessage $ Go L, sendMessage Shrink),
    ((lWinMask, xK_j ), sendMessage $ Go D, windows W.focusDown),
    ((lWinMask, xK_k ), sendMessage $ Go U, windows W.focusUp),
    ((lWinMask, xK_l ), sendMessage $ Go R, sendMessage Expand),
    ((lWinMask .|. shiftMask, xK_h ), 
        sendMessage $ BSP.ExpandTowards L, sendMessage Shrink),
    ((lWinMask .|. shiftMask,   xK_j ),
        sendMessage $ BSP.ExpandTowards D, windows W.swapDown),
    ((lWinMask .|. shiftMask,   xK_k ),
        sendMessage $ BSP.ExpandTowards U, windows W.swapUp),
    ((lWinMask .|. shiftMask,   xK_l ),
        sendMessage $ BSP.ExpandTowards R, sendMessage Expand),
    ((lWinMask .|. controlMask,   xK_h ), sendMessage $ Swap L, sendMessage Shrink),
    ((lWinMask .|. controlMask,   xK_j ), sendMessage $ Swap D, windows W.swapDown),
    ((lWinMask .|. controlMask,   xK_k ), sendMessage $ Swap U, windows W.swapUp),
    ((lWinMask .|. controlMask,   xK_l ), sendMessage $ Swap R, sendMessage Expand)
    
 ]


--  , ((rWinMask,                 xK_m ), withFocused (sendMessage . MergeAll))
--  , ((rWinMask,                 xK_u ), withFocused (sendMessage . UnMerge))
--  , ((rWinMask .|. shiftMask,    xK_k ), onGroup W.focusUp' )
--  , ((rWinMask .|. shiftMask,    xK_j ), onGroup W.focusDown' ) ]

myWindowNavKeys x = [
    ((rAltMask,                 xK_l ), sendMessage $ pullGroup R)
  , ((rAltMask,                 xK_h ), sendMessage $ pullGroup L)
  , ((rAltMask,                 xK_j ), sendMessage $ pullGroup D)
  , ((rAltMask,                 xK_k ), sendMessage $ pullGroup U)
  , ((rAltMask,                 xK_m ), withFocused (sendMessage . MergeAll))
  , ((rAltMask,                 xK_u ), withFocused (sendMessage . UnMerge))
  , ((rAltMask .|. lWinMask,    xK_k ), onGroup W.focusUp' )
  , ((rAltMask,                 xK_s ), sendMessage $ SwapWindow)
  , ((rAltMask .|. lWinMask,    xK_j ), onGroup W.focusDown' )
  , ((lWinMask,                 xK_d ), spawn "~/.xmonad/scripts/focusMouse.sh")
  , ((rWinMask,                 xK_d ), spawn "~/.xmonad/scripts/focusMouse.sh")
  , ((rWinMask .|. rAltMask,    xK_s ), layoutSplitScreen 2 (TwoPane 0.8 0.2))
  , ((rWinMask .|. rAltMask .|. shiftMask,    xK_s ), rescreen) ]

  {-, ((rAltMask .|. controlMask, xK_k ), focusUp )-}
  {-, ((rAltMask .|. controlMask, xK_j ), focusDown U) ]-}
myBspKeys x = [
    ((rWinMask,                 xK_l ), sendMessage $ Go R)
  , ((rWinMask,                 xK_h ), sendMessage $ Go L)
  , ((rWinMask,                 xK_j ), sendMessage $ Go D)
  , ((rWinMask,                 xK_k ), sendMessage $ Go U)
  , ((lWinMask,                 xK_l ), sendMessage $ Go R)
  , ((lWinMask,                 xK_h ), sendMessage $ Go L)
  , ((lWinMask,                 xK_j ), sendMessage $ Go D)
  , ((lWinMask,                 xK_k ), sendMessage $ Go U)
  , ((rWinMask .|. lWinMask,    xK_l ), sendMessage $ BSP.ExpandTowards R)
  , ((rWinMask .|. lWinMask,    xK_h ), sendMessage $ BSP.ExpandTowards L)
  , ((rWinMask .|. lWinMask,    xK_j ), sendMessage $ BSP.ExpandTowards D)
  , ((rWinMask .|. lWinMask,    xK_k ), sendMessage $ BSP.ExpandTowards U)
  {-, ((rWinMask             ,    xK_b ), sendMessage $ BSP.TreeBalance)-}
  , ((rWinMask .|. controlMask,   xK_l ), sendMessage $ Swap R)
  , ((rWinMask .|. controlMask,   xK_h ), sendMessage $ Swap L)
  , ((rWinMask .|. controlMask,   xK_j ), sendMessage $ Swap D)
  , ((rWinMask .|. controlMask,   xK_k ), sendMessage $ Swap U)


  , ((lWinMask .|. shiftMask,   xK_r ), sendMessage BSP.Rotate)
  , ((rWinMask .|. shiftMask,   xK_s ), sendMessage BSP.Swap)
  , ((lWinMask,                 xK_n ), sendMessage BSP.FocusParent)
  , ((lWinMask .|. shiftMask, xK_n ), sendMessage BSP.SelectNode)
  , ((lWinMask .|. shiftMask .|. controlMask, xK_n ), sendMessage BSP.MoveNode)
  , ((lWinMask,                 xK_a ), sendMessage BSP.Equalize)
  , ((lWinMask .|. shiftMask,   xK_a ), sendMessage BSP.Balance)
  , ((lWinMask .|. lAltMask,    xK_l ), sendMessage $ pullGroup R)
  , ((lWinMask .|. lAltMask,    xK_h ), sendMessage $ pullGroup L)
  , ((lWinMask .|. lAltMask,    xK_j ), sendMessage $ pullGroup D)
  , ((lWinMask .|. lAltMask,    xK_k ), sendMessage $ pullGroup U)
  , ((lWinMask,                 xK_m ), withFocused (sendMessage . MergeAll))
  , ((lWinMask .|. shiftMask,   xK_m ), withFocused (sendMessage . UnMerge))
  , ((lWinMask .|. controlMask .|. shiftMask,    xK_k ), onGroup W.focusUp' )
  , ((lWinMask .|. controlMask .|. shiftMask,    xK_j ), onGroup W.focusDown' )


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
    ((lWinMask .|. shiftMask, xK_s), spawn "snipit")
  , ((lAltMask .|. controlMask, xK_l), spawn "~/.xmonad/commands/lockscreen")
  , ((rWinMask, xK_f), spawn "~/.xmonad/commands/lock_mac")
  , ((lAltMask, xK_s), spawn "google-chrome http://sponge/lucky")
  , ((lWinMask, xK_s), spawn "~/swap.sh swap")
  , ((lWinMask, xK_b), sendMessage ToggleStruts )
  , ((lWinMask, xK_z), spawn "rofi -show window")
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
  , ((lWinMask .|. controlMask, xK_r), namedScratchpadAction scratchpads "hotkeys")
  , ((lWinMask .|. controlMask, xK_d), namedScratchpadAction scratchpads "chat")
  , ((lWinMask .|. controlMask, xK_c), namedScratchpadAction scratchpads "calendar") ]
promptKeys x = [
   ((lWinMask,                 xK_g),  spawn "$HOME/.xmonad/scripts/gmail.sh --xmonad" )
  ,((lWinMask,                 xK_u),  spawn "$HOME/.xmonad/scripts/gmail.sh --xmonad unread" ) ]
  {-, ((lWinMask,                 xK_g), spawn (runProcessWithInput "~/.xmonad/scripts/gmail.sh" ["--list"] ""))-}
  {-, ((lWinMask,                 xK_g),  gmailPrompt [("asdf", spawn "ls")] myXPConfig ) ]-}

{-data XMonad = XMonad-}
{--- | An xmonad prompt with a custom command list-}
{-gmailPrompt :: [(String, X())] -> XPConfig -> X ()-}
{-gmailPrompt commands c =-}
    {-mkXPrompt XMonad c (mkComplFunFromList' c) $-}
        {-fromMaybe (return ()) . (`lookup` commands)-}

{-gmailCommands = [ ("direct-to-me", safeSpawn "~/.xmonad/scripts/gmail.sh direct-to-me") ]-}

{-gmailLabels = runProcessWithInput "~/.xmonad/scripts/gmail.sh" ["--list"]-}



keysToDel x = []

defaultKeysToDel x = [
    (lWinMask, xK_w)
  , (lWinMask, xK_e)
  , (lWinMask, xK_r) 
  , (lWinMask, xK_h)
  , (lWinMask, xK_j)
  , (lWinMask, xK_k)
  , (lWinMask, xK_l) 
  , (lWinMask .|. shiftMask, xK_h)
  , (lWinMask .|. shiftMask, xK_j)
  , (lWinMask .|. shiftMask, xK_k)
  , (lWinMask .|. shiftMask, xK_l) ]
keysDefault = keys def
defaultKeys x = foldr M.delete (keysDefault x) (defaultKeysToDel x)

newKeys x = M.unions [ (defaultKeys x)
                     , (M.fromList(mySecondWorkspaceKeys x))
                     , (M.fromList(myThirdWorkspaceKeys x))
                     , (M.fromList(keysToAdd x))
                     , (M.fromList(myWindowNavKeys x))
                     , (M.fromList(myScreenNavKeys x))
                     , (M.fromList(myPerLayoutKeys x))
                     , (M.fromList(myBspKeys x))
                     , (M.fromList(promptKeys x))
                     , (M.fromList(scratchpadKeys x)) ]
myKeys x = foldr M.delete (newKeys x) (keysToDel x)



myAdditionalKeys = [
    ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")
  , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
  , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
  , ("<XF86MonBrightnessUp>", spawn "~/.xmonad/scripts/brightness.sh up")
  , ("<XF86MonBrightnessDown>", spawn "~/.xmonad/scripts/brightness.sh down") ]

myMouse x = [ ((lAltMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
myMouseBindings x = M.union (mouseBindings def x) (M.fromList (myMouse x))

myTerminal = "urxvt"

sBar :: String
sBar = "xmobar"
pp = case sBar of
  "xmobar" -> xmobarPP

myXmobarPrimaryConfig = "~/.config/xmonad/xmobarprimary"
myXmobarSecondaryConfig = "~/.config/xmonad/xmobarsecondary"

myPrimaryScreen :: Int
myPrimaryScreen = 0

myXMobarCommand :: Int -> String
myXMobarCommand s 
  | s == myPrimaryScreen = "xmobar -x " ++ show s ++ " " ++ myXmobarPrimaryConfig
  | otherwise           = "xmobar -x " ++ show s ++ " " ++ myXmobarSecondaryConfig

myStatusBar (S s) = statusBarPipe (myXMobarCommand s) $ (pure $ focusedScreenPP (S s))

myXPConfig = def {
    searchPredicate = fuzzyMatch
}

myStartupHook = composeAll [
      setWMName "LG3D"
    --, execScriptHook "start screensaver"
    --, execScriptHook "start xmobarpipes"
    --, execScriptHook "start drive"
    , execScriptHook "start feh"
    , execScriptHook "start notify-server"
    , execScriptHook "start synapse"
    , execScriptHook "start trayer"
    , execScriptHook "start blueman"
    ]

main = 
    xmonad 
      $ dynamicSBs myStatusBar
      $ withUrgencyHook LibNotifyUrgencyHook
      $ ewmh
      $ docks
      $ def {
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


