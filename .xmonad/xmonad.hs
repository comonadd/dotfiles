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
takeScreenshot = spawn "scrot"

openLauncher :: X()
openLauncher = spawn "bash -ci \"gmrun\""

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

myWorkspaces :: [(X11.KeySym, String)]
myWorkspaces = [
  (X11.xK_1, devWorkspaceLabel),
  (X11.xK_2, term1WorkspaceLabel),
  (X11.xK_3, term2WorkspaceLabel),
  (X11.xK_4, webWorkspaceLabel),
  (X11.xK_5, musicWorkspaceLabel),
  (X11.xK_6, chatWorkspaceLabel),
  (X11.xK_7, emailWorkspaceLabel),
  (X11.xK_8, term3WorkspaceLabel),
  (X11.xK_9, sysWorkspaceLabel)
  ]

-- termWorkspaceByNumber :: Int -> WorkspaceId
-- termWorkspaceByNumber n
--   | n == 1 = term1WorkspaceLabel
--   | n == 2 = term2WorkspaceLabel
--   | otherwise = term3WorkspaceLabel

-- selectRandomTermWorkspaceLabel :: IO WorkspaceId
-- selectRandomTermWorkspaceLabel = do
--   number <- randomRIO (1, 3) :: IO Int
--   let res = termWorkspaceByNumber $ number :: IO WorkspaceId
--   return res

-----------------
-- Layout hook --
-----------------

myLayoutHook = avoidStruts $ layoutHook def

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
      ((modm, X11.xK_r), recompileAndRestart),
      ((modm, X11.xK_c), kill),
      ((modm .|. X11.shiftMask, X11.xK_q), shutdown),
      ((modm .|. X11.shiftMask, X11.xK_r), reboot)
      ]

    volManagementKeys = [
      ((modm, X11.xK_equal), volUp),
      ((modm, X11.xK_minus), volDown),
      ((modm, X11.xK_m), volMute),
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
          ((modm .|. X11.shiftMask, key), windows $ StackSet.shift ws)
            | (key, ws) <- myWorkspaces]

    winManagementKeys = [
      ((modm, X11.xK_l),                   windowGo R False),
      ((modm, X11.xK_j),                   windowGo L False),
      ((modm, X11.xK_i),                   windowGo U False),
      ((modm, X11.xK_k),                   windowGo D False),
      ((modm .|. X11.shiftMask, X11.xK_l), windowSwap R False),
      ((modm .|. X11.shiftMask, X11.xK_j), windowSwap L False),
      ((modm .|. X11.shiftMask, X11.xK_i), windowSwap U False),
      ((modm .|. X11.shiftMask, X11.xK_k), windowSwap D False)
      ]

    otherKeys = [
      ((mod4Mask, xK_space),    sendMessage ToggleStruts),
      ((modm, X11.xK_y),        takeScreenshot),
      ((modm, X11.xK_p),        openLauncher),
      ((modm, X11.xK_b),        openBrowser),
      ((modm, X11.xK_Return),   openTerm),
      ((0, X11.xK_Scroll_Lock), toggleKbdBacklight)
      ]

-------------------
-- Configuration --
-------------------

myConfig = def {
  modMask = X11.mod4Mask,
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
