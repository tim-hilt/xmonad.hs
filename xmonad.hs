-- File: xmonad.hs
-- Author: Tim Hilt
-- Date created: 2020-04-12

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- Installed modules
import           XMonad
import           XMonad.Layout.Spiral
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Decoration
import           XMonad.Util.EZConfig           ( additionalKeys )
import           XMonad.Util.Types
import           XMonad.Hooks.EwmhDesktops
import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11

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
  spawn "autorandr --change"
  spawn "xset b off"

myNormalBorderColor :: String
myNormalBorderColor = "#222222"

myFocusedBorderColor :: String
myFocusedBorderColor = "#aa0000"

standardTheme = def { activeColor         = "#ff0000"
                    , activeBorderColor   = "#ff0000"
                    , activeTextColor     = "#ff0000"
                    , inactiveBorderColor = "#ffffff"
                    , inactiveColor       = "#ffffff"
                    , inactiveTextColor   = "#ffffff"
                    , urgentBorderColor   = "#ffff00"
                    , urgentColor         = "#ffff00"
                    , urgentTextColor     = "#ffff00"
                    , decoWidth           = 20
                    , decoHeight          = 20
                    }

data SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where

  shrink b (Rectangle _ _ dw dh) (Rectangle x y w h)
    | SideDecoration U <- b = Rectangle x (y + fi dh) w (h - dh)
    | SideDecoration R <- b = Rectangle x y (w - dw) h
    | SideDecoration D <- b = Rectangle x y w (h - dh)
    | SideDecoration L <- b = Rectangle (x + fi dw) y (w - dw) h

  pureDecoration b dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration U -> Rectangle x y w dh
      SideDecoration R -> Rectangle (x + fi (w - dw)) y dw h
      SideDecoration D -> Rectangle x (y + fi (h - dh)) w dh
      SideDecoration L -> Rectangle x y dw h
    | otherwise = Nothing

myLayouts =
  -- decoration shrinkText standardTheme (SideDecoration D)
  equalSpacing 60 6 0 1 (Tall 1 (3 / 100) (1 / 2))
    -- ||| equalSpacing 60 6 0 1 (Mirror (Tall 1 (3 / 100) (1 / 2)))
    ||| equalSpacing 60 0 0 1 (Full)
    ||| equalSpacing 60 6 0 1 (emptyBSP)
    ||| equalSpacing 60 6 0 1 (spiral (6 / 7))

myKeys =
  [ ((myModMask, xK_Return)              , spawn myTerminal)
  , ((myModMask .|. shiftMask, xK_Return), windows W.shiftMaster)
  , ((myModMask, xK_r)                   , spawn $ myTerminal ++ " -e ranger")
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
                              , borderWidth        = 1
                              , normalBorderColor  = myNormalBorderColor
                              , focusedBorderColor = myFocusedBorderColor
                              , modMask            = myModMask
                              , startupHook        = myStartup
                              , focusFollowsMouse  = False
                              , layoutHook         = myLayouts
                              }
    `additionalKeys` myKeys
