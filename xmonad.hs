-- File: xmonad.hs
-- Author: Tim Hilt
-- Date created: 2020-04-12

-- Installed modules
import           XMonad
import           XMonad.Layout.Spiral
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Util.EZConfig           ( additionalKeys )
import           XMonad.Hooks.EwmhDesktops
import           Graphics.X11.ExtraTypes.XF86

-- Qualified local imports
import qualified XMonad.StackSet               as W

-- Local modules
import           XMonad.Layout.EqualSpacing

myTerminal :: String
myTerminal = "alacritty"

myModMask :: KeyMask
myModMask = mod4Mask

myStartup :: X ()
myStartup = do
  spawn "hsetroot -cover ~/dev/dwm/assets/background.jpg"
  spawn "xset b off"

myNormalBorderColor :: String
myNormalBorderColor = "#222222"

myFocusedBorderColor :: String
myFocusedBorderColor = "#aa0000"

myLayouts =
  equalSpacing 60 6 0 1 (Tall 1 (3 / 100) (1 / 2))
    ||| equalSpacing 60 6 0 1 (emptyBSP)
    ||| equalSpacing 60 0 0 1 (Full)
    ||| equalSpacing 60 6 0 1 (spiral (6 / 7))
    ||| equalSpacing 60 6 0 1 (Mirror (Tall 1 (3 / 100) (1 / 2)))

myKeys =
  [ ((myModMask, xK_Return)              , spawn myTerminal)
  , ((myModMask .|. shiftMask, xK_Return), windows W.shiftMaster)
  , ((0, xF86XK_AudioMute)               , spawn "amixer set Master toggle -q")
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master unmute 3%- -q")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master unmute 3%+ -q")
  , ((0, xF86XK_MonBrightnessDown)       , spawn "light -U 5")
  , ((0, xF86XK_MonBrightnessUp)         , spawn "light -A 5")
  ]

main :: IO ()
main =
  do
      xmonad
    $                ewmhFullscreen
    $                ewmh def { terminal           = myTerminal
                              , normalBorderColor  = myNormalBorderColor
                              , focusedBorderColor = myFocusedBorderColor
                              , modMask            = myModMask
                              , startupHook        = myStartup
                              , focusFollowsMouse  = False
                              , layoutHook         = myLayouts
                              }
    `additionalKeys` myKeys
