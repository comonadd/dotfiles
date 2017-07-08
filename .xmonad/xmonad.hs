import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import XMonad
import XMonad.Config.Kde
import           XMonad.Actions.Navigation2D
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Hooks.ManageHelpers

------------------------
-- SH commands to use --
------------------------

recompileAndRestart :: X()
recompileAndRestart = spawn "xmonad --recompile && xmonad --restart"

shutdown :: X()
shutdown = spawn "sudo shutdown -h now"

reboot :: X()
reboot = spawn "sudo reboot"

takeScreenshot :: X()
takeScreenshot = spawn "~/Scripts/make_screenshot.sh"

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
          ((modm, key), windows $ W.greedyView ws)
            | (key, ws) <- myWorkspaces]
        wsShiftKeys = [
          ((modm .|. shiftMask, key), windows $ W.shift ws)
            | (key, ws) <- myWorkspaces]

    winManagementKeys = [
      ((modm, xK_l),                   windowGo R False),
      ((modm, xK_j),                   windowGo L False),
      ((modm, xK_i),                   windowGo U False),
      ((modm, xK_k),                   windowGo D False),
      ((modm .|. shiftMask, xK_l),     windowSwap R False),
      ((modm .|. shiftMask, xK_j),     windowSwap L False),
      ((modm .|. shiftMask, xK_i),     windowSwap U False),
      ((modm .|. shiftMask, xK_k),     windowSwap D False),
      ((modm, xK_Tab),                 windows W.focusDown),
      ((modm .|. shiftMask, xK_Tab),   windows W.focusUp)
      ]

    otherKeys = [
      ((modm, xK_space),    sendMessage NextLayout),
      ((modm, xK_t),         withFocused $ windows . W.sink),
      ((modm, xK_y),        takeScreenshot),
      --((modm, xK_p),        openLauncher),
      ((modm, xK_b),        openBrowser),
      ((modm, xK_Return),   openTerm),
      ((0, xK_Scroll_Lock), toggleKbdBacklight)
      ]

myManageHook :: Query (Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
    [
      [ isFullscreen --> doFullFloat ],
      [ className   =? c --> doFloat           | c <- myFloats ],
      [ title       =? t --> doFloat           | t <- myOtherFloats ]
    ]
  where myFloats      = ["MPlayer", "Gimp", "krunner", "plasmashell"]
        myOtherFloats = ["alsamixer"]

main :: IO()
main = xmonad kdeConfig
  {
  modMask = mod4Mask,
  terminal = "urxvtc",
  focusFollowsMouse = False,
  keys = myKeys,
  workspaces = map snd myWorkspaces,

  -- Border
  borderWidth = 0,
  focusedBorderColor = "#2E9AFE",
  normalBorderColor = "#000000"

  -- Hooks
  --manageHook = myManageHook
  -- layoutHook = myLayoutHook,
  -- handleEventHook = fullscreenEventHook,
  -- startupHook = myStartupHook
  }
