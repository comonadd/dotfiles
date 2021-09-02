import qualified Data.Map as Map
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

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

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
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
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

logout :: X()
logout = spawn "kill -9 -1"

takeScreenshot :: X()
takeScreenshot = spawn "~/Scripts/make_screenshot.sh"

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

toggleKbdBacklight :: X()
toggleKbdBacklight = spawn "~/Scripts/toggle_kbd_backlight.sh"

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

----------------
-- Workspaces --
----------------

myWorkspaces :: [(KeySym, String)]
myWorkspaces = [ (xK_1, "dev")
               , (xK_2, "web")
               , (xK_3, "etc")
               ]

--------------
-- Bindings --
--------------

myKeys :: XConfig Layout -> Map.Map (ButtonMask, KeySym) (X())
myKeys XConfig {modMask = modm} = Map.fromList $
  sysManagementKeys ++ volManagementKeys ++
  wsManagementKeys ++ winManagementKeys ++ otherKeys
  where
    sysManagementKeys = [
      ((modm, xK_r), recompileAndRestart),
      ((modm, xK_q), kill),
      ((modm .|. shiftMask, xK_q), logout),
      ((modm .|. shiftMask, xK_r), reboot)
      ]

    volManagementKeys = [
      ((modm, xK_equal), volUp),
      ((modm, xK_minus), volDown),
      ((modm, xK_m), volMute),
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
      ((modm, xK_l),                   windowGo R False),
      ((modm, xK_h),                   windowGo L False),
      ((modm, xK_k),                   windowGo U False),
      ((modm, xK_j),                   windowGo D False),
      ((modm .|. shiftMask, xK_l),     windowSwap R False),
      ((modm .|. shiftMask, xK_h),     windowSwap L False),
      ((modm .|. shiftMask, xK_k),     windowSwap U False),
      ((modm .|. shiftMask, xK_j),     windowSwap D False),
      ((modm, xK_Tab),                 windows W.focusDown),
      ((modm .|. shiftMask, xK_Tab),   windows W.focusUp),
      ((modm, xK_d),                   viewEmptyWorkspace),
      ((modm, xK_t),                   withFocused toggleFloat)
      ]

    otherKeys = [
      ((modm, xK_space),    sendMessage NextLayout),
      ((modm, xK_t),         withFocused $ windows . W.sink),
      ((modm, xK_y),        takeScreenshot),

      -- launcher
      ((modm, xK_p),        spawn "dmenu_run"),
      ((modm .|. shiftMask, xK_p),        spawn "gmrun"),

      ((modm, xK_b),        openBrowser),
      ((modm, xK_Return),   openTerm),
      ((0, xK_Scroll_Lock), toggleKbdBacklight),

      ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
      ]

myManageHook = composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , className =? "TeamViewer"     --> doFullFloat
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "dkesktop"       --> doIgnore
  , className =? "alsamixer"      --> doFloat
  , className =? "krunner"        --> doFloat
  , className =? "notification"        --> doFloat
  , className =? "plasmashell"    --> doFloat 
  , className =? "dialog"    --> doFloat 
  , className =? "file_progress"    --> doFloat 
  , className =? "error"    --> doFloat 
  , className =? "confirm"    --> doFloat 
  , isFullscreen                  --> doFullFloat
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

-- Defining a bunch of layouts, many that I don't use.
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

myBorderWidth = 1
sp = 10 -- Screen outer padding
wip = 5 -- Inner padding between windows
myLayoutHook = avoidStruts $ myLayouts
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


myBaseConfig = kdeConfig

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

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
      workspaces = map snd myWorkspaces,
      borderWidth = 0,
      focusedBorderColor = "#2E9AFE",
      normalBorderColor = "#000000",
      manageHook = manageHook kdeConfig <+> myManageHook,
      startupHook = startupHook kdeConfig <+> myStartupHook,
      handleEventHook = fullscreenEventHook,
      layoutHook = myLayoutHook
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
