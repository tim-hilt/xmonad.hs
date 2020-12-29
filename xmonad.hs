-- File: xmonad.hs
-- Author: Tim Hilt
-- Date created: 2020-04-12

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

-- Installed modules
import           XMonad
-- import           XMonad.Layout.Spiral
-- import           XMonad.Layout.BinarySpacePartition
-- import           XMonad.Layout.Tabbed
import           XMonad.Layout.Decoration
import           XMonad.Layout.NoBorders
import           XMonad.Layout.SimplestFloat
import           XMonad.Actions.FloatSnap
import           XMonad.Util.EZConfig           ( additionalKeys )
import           XMonad.Util.SpawnOnce
import           XMonad.Actions.GroupNavigation
import           XMonad.Hooks.EwmhDesktops
import           Graphics.X11.ExtraTypes.XF86
import           Control.Arrow                  ( second )

-- Qualified local imports
import qualified XMonad.StackSet               as W


-- equalSpacing-module begins here
equalSpacing
  :: Int -> Int -> Rational -> Int -> l a -> ModifiedLayout EqualSpacing l a
equalSpacing gap add mult min = ModifiedLayout (EqualSpacing gap add mult min)


data EqualSpacingMsg = MoreSpacing Int | LessSpacing Int deriving (Typeable)

instance Message EqualSpacingMsg

data EqualSpacing a = EqualSpacing
  { gap  :: Int
  , add  :: Int
  , mult :: Rational
  , min  :: Int
  }
  deriving Read


instance Show (EqualSpacing a) where
  show (EqualSpacing g a _ m) =
    "EqualSpacing " ++ show g ++ " " ++ show a ++ " " ++ show m


instance LayoutModifier EqualSpacing a where

  modifierDescription = show

  modifyLayout eqsp workspace screen = runLayout workspace $ shrinkScreen
    eqsp
    ((length $ W.integrate' $ W.stack workspace) - 1)
    screen

  pureModifier eqsp _ stck windows =
    ( map (second $ shrinkWindow eqsp ((length $ W.integrate' stck) - 1))
          windows
    , Nothing
    )

  pureMess eqsp msg
    | Just (MoreSpacing d) <- fromMessage msg = Just
    $ eqsp { gap = (d + (fi $ gap eqsp)) }
    | Just (LessSpacing d) <- fromMessage msg = Just
    $ eqsp { gap = max 0 (-d + (fi $ gap eqsp)) }
    | otherwise = Nothing


shrinkScreen :: EqualSpacing a -> Int -> Rectangle -> Rectangle
shrinkScreen (EqualSpacing gap add mult m) num (Rectangle x y w h) = Rectangle
  x
  y
  (w - fi sp)
  (h - fi sp)
  where sp = max m $ gap - (num * add)


shrinkWindow :: EqualSpacing a -> Int -> Rectangle -> Rectangle
shrinkWindow (EqualSpacing gap add mult m) num (Rectangle x y w h) = Rectangle
  (x + fi sp)
  (y + fi sp)
  (w - fi sp)
  (h - fi sp)
  where sp = max m $ gap - (num * add)

-- equalSpacing-module ends here

myTerminal :: String
myTerminal = "alacritty"

myModMask :: KeyMask
myModMask = mod4Mask

myStartup :: X ()
myStartup = do
  spawnOnce "hsetroot -cover ~/dev/dwm/assets/background.jpg &"
  spawnOnce "autorandr --change &"
  spawnOnce "xset b off &"
  spawnOnce "udiskie &"

myNormalBorderColor :: String
myNormalBorderColor = "#222222"

myFocusedBorderColor :: String
myFocusedBorderColor = "#aa0000"

myClickJustFocuses :: Bool
myClickJustFocuses = False

myLayouts =
  smartBorders
    $   equalSpacing 60 0 0 1 (Tall 1 (3 / 100) (1 / 2))
    ||| equalSpacing 60 0 0 1 Full
    ||| simplestFloat
    -- ||| equalSpacing 60 0 0 1 emptyBSP
    -- ||| equalSpacing 60 0 0 1 (spiral (6 / 7))


centerWindow :: Window -> X ()
centerWindow win = do
  (_, W.RationalRect _ _ w h) <- floatLocation win
  windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
  return ()


myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((myModMask, xK_Return)              , spawn myTerminal)
  , ((myModMask .|. shiftMask, xK_Return), windows W.shiftMaster)
  , ((myModMask, xK_r)                   , spawn $ myTerminal ++ " -e ranger")
  , ((myModMask, xK_Tab)                 , nextMatch History (return True))
  , ((myModMask, xK_c)                   , withFocused centerWindow)
  , ((myModMask, xK_a), spawn "python ~/.xmonad/move_window.py")
  , ((myModMask, xK_Left)                , withFocused $ snapMove L Nothing)
  , ((myModMask, xK_Right)               , withFocused $ snapMove R Nothing)
  , ((myModMask, xK_Up)                  , withFocused $ snapMove U Nothing)
  , ((myModMask, xK_Down)                , withFocused $ snapMove D Nothing)
  , ((myModMask .|. shiftMask, xK_Left)  , withFocused $ snapShrink R Nothing)
  , ((myModMask .|. shiftMask, xK_Right) , withFocused $ snapGrow R Nothing)
  , ((myModMask .|. shiftMask, xK_Up)    , withFocused $ snapShrink D Nothing)
  , ((myModMask .|. shiftMask, xK_Down)  , withFocused $ snapGrow D Nothing)
  , ((0, xF86XK_AudioMute)               , spawn "amixer set Master toggle -q")
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master unmute 3%- -q")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master unmute 3%+ -q")
  , ((0, xF86XK_MonBrightnessDown)       , spawn "light -U 5")
  , ((0, xF86XK_MonBrightnessUp)         , spawn "light -A 5")
  , ((0, xF86XK_AudioPlay)               , spawn "playerctl play-pause")
  , ((0, xF86XK_AudioNext)               , spawn "playerctl next")
  , ((0, xF86XK_AudioPrev)               , spawn "playerctl previous")
  , ((0, xK_Print)                       , spawn "spectacle")
  ]

main :: IO ()
main =
  do
      xmonad
    -- $                ewmhFullscreen
    $                ewmh def { terminal           = myTerminal
                              , borderWidth        = 2
                              , normalBorderColor  = myNormalBorderColor
                              , focusedBorderColor = myFocusedBorderColor
                              , modMask            = myModMask
                              , clickJustFocuses   = myClickJustFocuses
                              , startupHook        = myStartup
                              , focusFollowsMouse  = False
                              , layoutHook         = myLayouts
                              , logHook            = historyHook
                              }
    `additionalKeys` myKeys
