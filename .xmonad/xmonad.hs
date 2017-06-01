{- |
Module        :  hs
Description   :  The XMonad WM main configuration file
Copyright     :  (c) Dmitry Guzeev <dmitry.guzeev@yahoo.com>
License       :  MIT license

Maintainer    :  dmitry.guzeev@yahoo.com
Stability     :  unstable
Portability   :  non-portable (XMonad only works on X11)
-}

module Main where

import qualified Data.Map                    as Map
import qualified Graphics.X11                as X11
import qualified System.Cmd
import           System.Random

import           XMonad
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet             as StackSet
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed

------------------------
-- SH commands to use --
------------------------

xmobarCmd :: String
xmobarCmd = "xmobar ~/.xmonad/xmobar.hs"

recompileAndRestart :: X()
recompileAndRestart = spawn "xmonad --recompile && xmonad --restart"

logout :: X()
logout = spawn "killall -9 -u $USER"

shutdown :: X()
shutdown = spawn "sudo shutdown -h now"

reboot :: X()
reboot = spawn "sudo reboot"

takeScreenshot :: X()
takeScreenshot = spawn "~/Scripts/make_screenshot.sh"

openLauncher :: X()
openLauncher = spawn "synapse"

openBrowser :: X()
openBrowser = spawn "google-chrome-stable"

openTerm :: X()
openTerm = spawn "urxvtc"

volUp :: X()
volUp = spawn "amixer sset Master 5%+"

volDown :: X()
volDown = spawn "amixer sset Master 5%-"

volMute :: X()
volMute = spawn "amixer sset Master toggle"

toggleKbdBacklight :: X()
toggleKbdBacklight = spawn "~/Scripts/toggle_kbd_backlight.sh"

---------------------------
-- Configuration options --
---------------------------

xmobarTitleColor :: String
xmobarTitleColor = "green"

xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "orange"

----------------
-- Workspaces --
----------------

devWorkspaceLabel :: WorkspaceId
devWorkspaceLabel = "[Development]"

term1WorkspaceLabel :: WorkspaceId
term1WorkspaceLabel = "[Terminal #1]"

term2WorkspaceLabel :: WorkspaceId
term2WorkspaceLabel = "[Terminal #2]"

webWorkspaceLabel :: WorkspaceId
webWorkspaceLabel = "[Web]"

musicWorkspaceLabel :: WorkspaceId
musicWorkspaceLabel = "[Music]"

chatWorkspaceLabel :: WorkspaceId
chatWorkspaceLabel = "[Chat]"

emailWorkspaceLabel :: WorkspaceId
emailWorkspaceLabel = "[Email]"

term3WorkspaceLabel :: WorkspaceId
term3WorkspaceLabel = "[Terminal #3]"

sysWorkspaceLabel :: WorkspaceId
sysWorkspaceLabel = "[System]"

myWorkspaces :: [(KeySym, String)]
myWorkspaces = [
  (xK_1, devWorkspaceLabel),
  (xK_2, term1WorkspaceLabel),
  (xK_3, term2WorkspaceLabel),
  (xK_4, webWorkspaceLabel),
  (xK_5, musicWorkspaceLabel),
  (xK_6, chatWorkspaceLabel),
  (xK_7, emailWorkspaceLabel),
  (xK_8, term3WorkspaceLabel),
  (xK_9, sysWorkspaceLabel)
  ]

-----------------
-- Layout hook --
-----------------

tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

myLayoutHook = avoidStruts (
  ThreeColMid 1 (3/100) (1/2) |||
  Tall 1 (3/100) (1/2) |||
  Mirror (Tall 1 (3/100) (1/2)) |||
  tabbed shrinkText tabConfig |||
  Full |||
  spiral (6/7)) |||
  noBorders (fullscreenFull Full)

-----------------
-- Manage hook --
-----------------

myManageHook = composeAll . concat $ [
  [manageDocks],
  [isFullscreen --> doFullFloat],
  [manageHook def]
  ]

------------------
-- Startup hook --
------------------

myStartupHook :: X()
myStartupHook = do
  setWMName "LG3D"
  spawnOnce "urxvtd -o -f -q"

----------
-- Keys --
----------

myKeys :: XConfig Layout -> Map.Map (ButtonMask, KeySym) (X())
myKeys XConfig {modMask = modm} = Map.fromList $
  sysManagementKeys ++ volManagementKeys ++
  wsManagementKeys ++ winManagementKeys ++ otherKeys
  where
    sysManagementKeys = [
      ((modm, xK_r), recompileAndRestart),
      ((modm, xK_c), kill),
      ((modm .|. shiftMask, xK_q), shutdown),
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
          ((modm, key), windows $ StackSet.greedyView ws)
            | (key, ws) <- myWorkspaces]
        wsShiftKeys = [
          ((modm .|. shiftMask, key), windows $ StackSet.shift ws)
            | (key, ws) <- myWorkspaces]

    winManagementKeys = [
      ((modm, xK_l),                   windowGo R False),
      ((modm, xK_j),                   windowGo L False),
      ((modm, xK_i),                   windowGo U False),
      ((modm, xK_k),                   windowGo D False),
      ((modm .|. shiftMask, xK_l), windowSwap R False),
      ((modm .|. shiftMask, xK_j), windowSwap L False),
      ((modm .|. shiftMask, xK_i), windowSwap U False),
      ((modm .|. shiftMask, xK_k), windowSwap D False),
      ((modm, xK_Tab),                 windows StackSet.focusDown),
      ((modm .|. shiftMask, xK_Tab),   windows StackSet.focusUp)
      ]

    otherKeys = [
      ((modm, xK_space),    sendMessage NextLayout),
      ((modm, xK_t),         withFocused $ windows . StackSet.sink),
      ((modm, xK_y),        takeScreenshot),
      ((modm, xK_p),        openLauncher),
      ((modm, xK_b),        openBrowser),
      ((modm, xK_Return),   openTerm),
      ((0, xK_Scroll_Lock), toggleKbdBacklight)
      ]

-------------------
-- Configuration --
-------------------

myConfig = def {
  modMask = mod4Mask,
  terminal = "urxvtc",
  focusFollowsMouse = False,
  keys = myKeys,
  workspaces = map snd myWorkspaces,

  -- Border
  borderWidth = 0,
  focusedBorderColor = "#2E9AFE",
  normalBorderColor = "#000000",

  -- Hooks
  layoutHook = myLayoutHook,
  handleEventHook = fullscreenEventHook,
  manageHook = myManageHook,
  startupHook = myStartupHook
  }

-----------------------
-- The main function --
-----------------------

main :: IO()
main = do
  xmproc <- spawnPipe xmobarCmd
  xmonad $ myConfig {
  logHook = dynamicLogWithPP xmobarPP {
              ppOutput = hPutStrLn xmproc,
              ppTitle = xmobarColor "darkgreen"  "" . shorten 32
            }
          }
