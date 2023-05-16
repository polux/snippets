-- Copyright 2023 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

import XMonad
import qualified Data.Map as M
import XMonad.Hooks.SetWMName
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ResizableTile
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (filterOutWs)
import XMonad.Hooks.DynamicLog
import XMonad.Actions.WindowBringer
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import Data.List

main = do
  xmproc <- spawnPipe "/home/polux/.local/bin/xmobar"
  xmonad . docks $ def
    { terminal = term
    , keys = \c -> myKeys c `M.union` keys def c
    , modMask = mod4Mask
    , manageHook = manageHook def <+> myManageHook
    , layoutHook = avoidStruts . windowNavigation . smartBorders $ myLayouts
    , workspaces = myWorkspaces
    , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  , ppSort = fmap (. filterOutWs [scratchpadWorkspaceTag]) $ ppSort xmobarPP
                  }
    , startupHook = setWMName "LG3D"
    }

-- terminals
term = "sakura"
scratchTerm = term ++ " --name scratchshell"

isMixer s = "Volume Control" `isInfixOf` s

-- scratch pads
pads =
 [ NS "shell" scratchTerm (resource =? "scratchshell")
     (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
 , NS "mixer" "pavucontrol" (className =? "Pavucontrol")
     (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
 ]

myLayouts = (Full ||| rtall ||| Mirror rtall)
  where rtall = ResizableTall 1 (3/100) (1/2) []

myWorkspaces = ["code","web","web2"] ++ map show [4..8] ++ ["demo"]

myManageHook = composeAll
  [ className  =? "Firefox"       --> doShift "web"
  , className  =? "Google-Chrome" --> doShift "web"
  , className  =? "Xchat"         --> doShift "chat"
  , isFullscreen                  --> doFullFloat
  , namedScratchpadManageHook pads]

myKeys (XConfig {modMask = modm}) = M.fromList $
  [ ((modm, xK_b), sendMessage ToggleStruts)
  -- xlock
  , ((modm .|. shiftMask, xK_l), spawn "/usr/bin/cinnamon-screensaver-command -l")
  -- scratch pad
  , ((modm, xK_s  ), namedScratchpadAction pads "shell")
  , ((modm, xK_bracketright), namedScratchpadAction pads "mixer")
  -- window navigation
  , ((modm,                 xK_Right), sendMessage $ Go R)
  , ((modm,                 xK_Left ), sendMessage $ Go L)
  , ((modm,                 xK_Up   ), sendMessage $ Go U)
  , ((modm,                 xK_Down ), sendMessage $ Go D)
  , ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
  , ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
  , ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
  , ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)
  -- window bringer
  , ((modm .|. shiftMask, xK_g), gotoMenu)
  , ((modm .|. shiftMask, xK_b), bringMenu)
  -- resizable tall
  , ((modm, xK_a), sendMessage MirrorShrink)
  , ((modm, xK_z), sendMessage MirrorExpand)
  -- rebin gmrun to super-P
  , ((modm, xK_p), spawn "gmrun")
  , ((modm .|. shiftMask, xK_s), spawn "/home/polux/projects/imgur-screenshot/imgur-screenshot -s -e true -i '/home/polux/.local/bin/pinta %img'")
  ]
