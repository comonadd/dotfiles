import qualified Data.Monoid as Monoid
import Text.Printf
import qualified Data.Map as M
import Data.Maybe (fromJust)

import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.Navigation2D
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.MouseResize
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.ManageHelpers
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.FadeInactive

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS, FULL))
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import XMonad.Layout.MultiColumns
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))

-- Terminal to open on Super+Enter
myTerminal = "gnome-terminal"
myBrowser = "firefox"

myFont :: String
myFont = "xft:Ubuntu Mono:weight=bold:pixelsize=14:antialias=true:hinting=true"
myBorderWidth = 1
-- Color of current window title in xmobar.
xmobarTitleColor = "#C678DD"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#51AFEF"
-- Color of the border on windows when focused
myFocusedBorderColor = "#2E9AFE"
-- When not in focus
myNormalBorderColor = "#000000"
-- Color of the XMobar workspace that's currently active
actWsColor = "#c792ea"
-- Color of XMobar workspaces that aren't active
inactWsColor = "#82AAFF"
-- Color of XMobar separator (should be synced with the one in xmobar.hs)
sepColor = "#666666"
-- XMobar current window title color
actWinTitleColor = "#b3afc2"
myBaseConfig = gnomeConfig
-- Tab layout settings
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }
myFocusFollowsMouse = True

------------------------
-- SH commands to use --
------------------------

compositorCommand = "picom --experimental-backends --backend glx --xrender-sync-fence --config ~/.config/compton.conf"

recompileAndRestart :: X()
recompileAndRestart = spawn "xmonad --recompile && xmonad --restart"

shutdown :: X()
shutdown = spawn "sudo shutdown -h now"

-- Needs "reboot" to be a runnable command for non-sudo users
reboot :: X()
reboot = spawn "sudo reboot"

logout :: X()
logout = spawn "kill -9 -1"

lockScreen :: X()
lockScreen = spawn "dm-tool switch-to-greeter"

takeScreenshot :: X()
takeScreenshot = spawn "~/Scripts/make-screenshot.sh"

openBrowser :: X()
openBrowser = spawn myBrowser

openTerm :: X()
openTerm = spawn myTerminal

openLauncher :: X()
openLauncher = spawn "albert show"

volUp :: X()
volUp = spawn "amixer sset Master 5%+"

volDown :: X()
volDown = spawn "amixer sset Master 5%-"

volMute :: X()
volMute = spawn "amixer sset Master toggle"

printHelp = spawn ("echo \"" ++ help ++ "\" | xmessage -file -")

-------------
-- Helpers --
-------------

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

----------------
-- Workspaces --
----------------

{-
 - Super+k will automatically bind to switching to a kth workspace
 - Super+Shift+k moves currently focused window to a kth workspace
-}
wsSys :: WorkspaceId
wsSys = "[1:sys]"
wsDev :: WorkspaceId
wsDev = "[2:dev]"
wsWeb :: WorkspaceId
wsWeb = "[3:web]"
wsIm :: WorkspaceId
wsIm = "[4:im]"
wsEtc :: WorkspaceId
wsEtc = "[5:etc]"
myWorkspaces :: [WorkspaceId]
myWorkspaces = [wsSys, wsDev, wsWeb, wsIm, wsEtc]

--------------
-- Bindings --
--------------

sysManagementKeys = [ ("M-r", recompileAndRestart)
                    , ("M-q", kill)
                    , ("M-S-l", logout)
                    , ("M-S-n", lockScreen)
                    , ("M-S-q", reboot) ]
volManagementKeys = [ ("<XF86AudioPlay>", spawn "mocp --play")
                    , ("<XF86AudioPrev>", spawn "mocp --previous")
                    , ("<XF86AudioNext>", spawn "mocp --next")
                    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
                    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
                    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
                    , ("<XF86HomePage>", spawn (myBrowser ++ "https://google.com"))
                    , ("<XF86Search>", spawn "dm-websearch")
                    , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
                    , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
                    , ("<XF86Eject>", spawn "toggleeject")
                    , ("<Print>", spawn "dm-maim") ]
indexedWS :: [(Integer, WorkspaceId)]
indexedWS = zip [1..] myWorkspaces
wsManagementKeys = wsSwitchKeys ++ wsShiftKeys
  where
    -- Switch to a kth workspace
    wsSwitchKeys =
      (map (\(idx, ws) -> (printf "M-%d" idx, windows $ W.greedyView ws)) indexedWS)
    -- Shift focused window to a workspace
    wsShiftKeys =
      (map (\(idx, ws) -> (printf "M-S-%d" idx, windows $ W.shift ws)) indexedWS)
winManagementKeys = [ ("M-m",                   windows W.focusMaster)
                    ,  ("M-k",                   windows W.focusUp)
                    ,  ("M-j",                   windows W.focusDown)
                    ,  ("M-S-m",     windows W.swapMaster)
                    ,  ("M-S-k",     windows W.swapUp)
                    ,  ("M-S-j",     windows W.swapDown)
                    ,  ("M-<Tab>",                 windows W.focusDown)
                    ,  ("M-S-<Tab>",   windows W.focusUp)
                    ,  ("M-d",                   viewEmptyWorkspace)
                    ,  ("M-t",                   withFocused toggleFloat) ]
appLauncherKeys = [ ("M-b",        openBrowser)
                  , ("M-<Return>",   openTerm)
                  , ("M-p",          openLauncher) ]
otherKeys = [ ("M-<Space>",    sendMessage NextLayout)
            , ("M-y",        takeScreenshot)
            , ("M-S-/", printHelp) ]
myKeys :: [(String, X())]
myKeys =
  sysManagementKeys ++ volManagementKeys ++
  wsManagementKeys ++ winManagementKeys ++
  appLauncherKeys ++ otherKeys

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


myManageHook = composeAll
  [ resource  =? "desktop_window"            --> doIgnore
  , resource  =? "dkesktop"                  --> doIgnore
  , className =? "MPlayer"                   --> doFloat
  , className =? "Gimp"                      --> doFloat
  , className =? "TeamViewer"                --> doFullFloat
  , className =? "alsamixer"                 --> doFloat
  , className =? "Gmrun"                     --> doCenterFloat
  , className =? "krunner"                   --> doFloat
  , className =? "notification"              --> doFloat
  , className =? "plasmashell"               --> doFloat
  , className =? "dialog"                    --> doFloat
  , className =? "file_progress"             --> doFloat
  , className =? "error"                     --> doFloat
  , className =? "confirm"                   --> doFloat
  , className =? "Galculator"                --> doCenterFloat
  , className =? "Steam"                     --> doCenterFloat
  , className =? "Gimp"                      --> doCenterFloat
  , className =? "Vlc"                       --> doFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  -- Workspaces
  , className =? "Google-chrome"             --> doShift wsWeb
  , className =? "firefox"                   --> doShift wsWeb
  , className =? "Chromium"                  --> doShift wsWeb
  , className =? "TelegramDesktop"           --> doShift wsIm
  -- Other
  , isDialog                                 --> doCenterFloat
  , isFullscreen                             --> doFullFloat
  ]

myManageHooks = [myManageHook, manageHook myBaseConfig]

-------------
-- Layouts --
-------------

-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- The default number of windows in the master pane
nmaster = 1
-- Percent of screen to increment by when resizing panes
delta   = 3/100
-- Default proportion of screen occupied by master pane
ratio   = 1/2
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
-- Tall: The default layout with the most different usecases
tall     = renamed [Replace "tall"]
           $ withBorder myBorderWidth
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall nmaster delta ratio []
-- Fullscreen: Browser, IDE with multiple panes, etc
full     = noBorders Full
tabs     = renamed [Replace "tabs"]
           $ withBorder myBorderWidth
           $ avoidStruts
           $ noBorders
           $ tabbed shrinkText myTabTheme
mirroredTall = withBorder myBorderWidth
         $ avoidStruts
         $ limitWindows 8
         $ mySpacing 4
         $ Mirror (Tall nmaster delta ratio)
-- Three columns, useful for programming
threeCol = withBorder myBorderWidth
         $ avoidStruts
         $ windowNavigation
         $ limitWindows 6
         $ mySpacing 4
         $ ThreeCol nmaster delta ratio
-- Multiple columns, useful for displaying logs & stdout
myMultiCol = withBorder myBorderWidth
           $ avoidStruts
           $ limitWindows 6
           $ mySpacing 4
           $ multiCol [1] 1 0.01 (-0.5)

myLayouts = tall ||| full ||| mirroredTall ||| threeCol ||| myMultiCol ||| tabs

myLayoutHook = avoidStruts
             $ myLayouts


myStartupHook :: X()
myStartupHook = do
  setWMName "LG3D"
  spawnOnce "nitrogen --restore &"
  spawnOnce compositorCommand
  spawnOnce "albert &"


toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

fadeMostInactives :: Rational -> X ()
fadeMostInactives = fadeOutLogHook . fadeIf (isUnfocused <&&> noneOf qs)
  where noneOf = fmap not . foldr1 (<||>)
        qs = [isFullscreen, className =? "Cardapio", className =? "Gimp"]

onAllEvents pp = do
  fadeMostInactives 0.95
  dynamicLogWithPP pp

main :: IO()
main = do
  -- Respawn xmobar (kill running processes if they exist)
  xmproc <- spawnPipe "killall -9 xmobar 2>/dev/null 1>/dev/null ; xmobar ~/.xmonad/xmobar.hs"
  xmonad $ ewmh $ myBaseConfig
    { modMask = mod4Mask
      -- Basic
      , terminal = myTerminal
      , focusFollowsMouse = myFocusFollowsMouse
      , workspaces = myWorkspaces
      , borderWidth = myBorderWidth
      , focusedBorderColor = myFocusedBorderColor
      , normalBorderColor = myNormalBorderColor
      -- Bindings
      , mouseBindings = myMouseBindings
      -- Hooks & layouts
      , manageHook = foldr1 (<+>) myManageHooks
      , startupHook = startupHook myBaseConfig <+> myStartupHook
      , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
      , layoutHook = myLayoutHook
      -- XMobar communication
      , logHook = onAllEvents $ defaultPP
        { ppOutput = \x -> hPutStrLn xmproc x
        -- Current workspace
        , ppCurrent = xmobarColor actWsColor "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ actWsColor ++ ">") "</box>"
        -- Visible but not current workspace
        , ppVisible = xmobarColor actWsColor ""
        -- Hidden workspaces
        , ppHidden = xmobarColor inactWsColor "" . wrap ("<box type=Top width=2 mt=2 color=" ++ inactWsColor ++ ">") "</box>"
        -- Hidden workspaces (no windows)
        , ppHiddenNoWindows = xmobarColor inactWsColor ""
        -- Title of active window
        --, ppTitle = xmobarColor actWinTitleColor "" . shorten 60
        , ppTitle = \x -> ""
        -- Separator character
        , ppSep = ("<fc=" ++ sepColor ++ "> <fn=1>|</fn> </fc>")
        -- Urgent workspace
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
        -- # of windows current workspace
        , ppExtras  = [windowCount]
        -- order of things in xmobar
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }
    } `additionalKeysP` myKeys


help :: String
help = unlines ["The default modifier key is 'super'. Default keybindings:",
  "",
  "-- launching and killing programs",
  "mod-Shift-Enter  Launch xterminal",
  "mod-p            Launch dmenu",
  "mod-Shift-p      Launch gmrun",
  "mod-Shift-c      Close/kill the focused window",
  "mod-Space        Rotate through the available layout algorithms",
  "mod-Shift-Space  Reset the layouts on the current workSpace to default",
  "mod-n            Resize/refresh viewed windows to the correct size",
  "",
  "-- move focus up or down the window stack",
  "mod-Tab        Move focus to the next window",
  "mod-Shift-Tab  Move focus to the previous window",
  "mod-j          Move focus to the next window"]
