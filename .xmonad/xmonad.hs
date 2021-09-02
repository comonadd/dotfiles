import qualified Data.Monoid as Monoid
import XMonad
import XMonad.Config.Kde
import XMonad.Actions.Navigation2D
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Actions.MouseResize
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Actions.UpdatePointer
import Data.Maybe (fromJust)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
  
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS, FULL))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

myTerminal = "gnome-terminal"
myBorderWidth = 1
-- Color of current window title in xmobar.
xmobarTitleColor = "#C678DD"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#51AFEF"

------------------------
-- SH commands to use --
------------------------

recompileAndRestart :: X()
recompileAndRestart = spawn "xmonad --recompile && xmonad --restart"

shutdown :: X()
shutdown = spawn "sudo shutdown -h now"

-- Needs "reboot" to be a runnable command for non-sudo users
reboot :: X()
reboot = spawn "sudo reboot"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
 
logout :: X()
logout = spawn "kill -9 -1"

lockScreen :: X()
lockScreen = spawn "dm-tool switch-to-greeter"

takeScreenshot :: X()
takeScreenshot = spawn "~/Scripts/make-screenshot.sh"

openBrowser :: X()
openBrowser = spawn "firefox"

openTerm :: X()
openTerm = spawn myTerminal

volUp :: X()
volUp = spawn "amixer sset Master 5%+"

volDown :: X()
volDown = spawn "amixer sset Master 5%-"

volMute :: X()
volMute = spawn "amixer sset Master toggle"

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

printHelp = spawn ("echo \"" ++ help ++ "\" | xmessage -file -")

----------------
-- Workspaces --
----------------

wsSys :: WorkspaceId
wsSys = "1:sys"
wsDev :: WorkspaceId
wsDev = "2:dev"
wsWeb :: WorkspaceId
wsWeb = "3:web"
wsEtc :: WorkspaceId
wsEtc = "4:etc"
myWorkspaces :: [(KeySym, String)]
myWorkspaces = [ (xK_1, wsSys)
               , (xK_2, wsDev)
               , (xK_3, wsWeb)
               , (xK_4, wsEtc)
               ]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

--------------
-- Bindings --
--------------

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X())
myKeys XConfig {modMask = modm} = M.fromList $
  sysManagementKeys ++ volManagementKeys ++
  wsManagementKeys ++ winManagementKeys ++ otherKeys
  where
    sysManagementKeys = [
      ((modm, xK_r), recompileAndRestart),
      ((modm, xK_q), kill),
      ((modm .|. shiftMask, xK_q), logout),
      ((modm .|. shiftMask, xK_n), lockScreen),
      ((modm .|. shiftMask, xK_r), reboot)
      ]

    volManagementKeys = [
      ((modm, xK_equal), volUp),
      ((modm, xK_minus), volDown),
      -- Media keyboard keys
      ((0, 0x1008FF12), volMute),
      ((0, 0x1008FF11), volDown),
      ((0, 0x1008FF13), volUp)
      ]

    wsManagementKeys = wsSwitchKeys ++ wsShiftKeys
      where
        wsSwitchKeys = [
          ((modm, key), windows $ W.greedyView ws)
            | (key, ws) <- myWorkspaces]
        wsShiftKeys = [
          ((modm .|. shiftMask, key), windows $ W.shift ws)
            | (key, ws) <- myWorkspaces]

    winManagementKeys = [
      ((modm, xK_m),                   windows W.focusMaster),
      ((modm, xK_k),                   windows W.focusUp),
      ((modm, xK_j),                   windows W.focusDown),
      ((modm .|. shiftMask, xK_m),     windows W.swapMaster),
      ((modm .|. shiftMask, xK_k),     windows W.swapUp),
      ((modm .|. shiftMask, xK_j),     windows W.swapDown),
      ((modm, xK_Tab),                 windows W.focusDown),
      ((modm .|. shiftMask, xK_Tab),   windows W.focusUp),
      ((modm, xK_d),                   viewEmptyWorkspace),
      ((modm, xK_t),                   withFocused toggleFloat)
      ]

    otherKeys = [
      ((modm, xK_space),    sendMessage NextLayout),
      ((modm, xK_t),        withFocused $ windows . W.sink),
      ((modm, xK_y),        takeScreenshot),

      -- launcher
      --((modm, xK_p),        spawn "dmenu_run"),
      --((modm, xK_p),        spawn "albert"),
      --((modm .|. shiftMask, xK_p),        spawn "gmrun"),

      ((modm, xK_b),        openBrowser),
      ((modm, xK_Return),   openTerm),

      ((modm .|. shiftMask, xK_slash), printHelp)
      ]


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
  [ className =? "MPlayer"                   --> doFloat
  , className =? "Gimp"                      --> doFloat
  , className =? "TeamViewer"                --> doFullFloat
  , resource  =? "desktop_window"            --> doIgnore
  , resource  =? "dkesktop"                  --> doIgnore
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
  -- Fullscreen float
  , isFullscreen                             --> doFullFloat
  -- Workspaces
  , className =? "Google-chrome"             --> doShift wsWeb
  , className =? "firefox"                   --> doShift wsWeb
  , className =? "Chromium"                  --> doShift wsWeb
  , className =? "TelegramDesktop"           --> doShift wsEtc
  ]

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Layouts.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabTheme

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

myLayoutHook = avoidStruts
             $ mkToggle (NOBORDERS ?? FULL ?? EOT)
             $ myLayouts
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
    myLayouts = withBorder myBorderWidth tall
               ||| Full
               ||| noBorders tabs
               ||| floats


myStartupHook :: X()
myStartupHook = do
  setWMName "LG3D"
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawnOnce "albert &"


myBaseConfig = kdeConfig

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
 
 {-
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices
  -}

myFocusedBorderColor = "#2E9AFE"
myNormalBorderColor = "#000000"
actWsColor = "#c792ea"
inactWsColor = "#82AAFF"
sepColor = "#666666"
actWinTitleColor = "#b3afc2"

main :: IO()
main = do
  -- Respawn xmobar (kill running processes if they exist)
  xmproc <- spawnPipe "killall -9 xmobar 2>/dev/null 1>/dev/null ; xmobar ~/.xmonad/xmobar.hs"
  xmonad $ docks myBaseConfig
    {
      modMask = mod4Mask,
      terminal = myTerminal,
      focusFollowsMouse = False,
      keys = myKeys,
      mouseBindings      = myMouseBindings,
      workspaces = map snd myWorkspaces,
      borderWidth = myBorderWidth,
      focusedBorderColor = myFocusedBorderColor,
      normalBorderColor = myNormalBorderColor,
      manageHook = manageHook kdeConfig <+> myManageHook,
      startupHook = startupHook kdeConfig <+> myStartupHook,
      handleEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook,
      layoutHook = myLayoutHook,
      logHook = dynamicLogWithPP $ xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc x
        -- Current workspace
        , ppCurrent = xmobarColor actWsColor "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ actWsColor ++ ">") "</box>"
        -- Visible but not current workspace
        , ppVisible = xmobarColor actWsColor "" 
        -- Hidden workspaces
        , ppHidden = xmobarColor inactWsColor "" . wrap ("<box type=Top width=2 mt=2 color=" ++ inactWsColor ++ ">") "</box>" 
        -- Title of active window
        , ppTitle = xmobarColor actWinTitleColor "" . shorten 60
        -- Separator character
        , ppSep = ("<fc=" ++ sepColor ++ "> <fn=1>|</fn> </fc>")
        -- Urgent workspace
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            
        -- # of windows current workspace
        , ppExtras  = [windowCount]                                     
        -- order of things in xmobar
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                    
        }
    }


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
