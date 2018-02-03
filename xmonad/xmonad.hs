import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, hPutStrLn, runProcessWithInput)

-- Layouts
import XMonad.Layout.Spacing(smartSpacing)
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders

-- Shutdown commands and keys
import Data.Map(fromList)
import XMonad.Prompt
import XMonad.Prompt.XMonad
import System.Exit(ExitCode(ExitSuccess), exitWith)
import XMonad.Util.EZConfig(additionalKeys, removeKeys)

-- Brightness and audio keys
import Graphics.X11.ExtraTypes.XF86

import Data.List(elemIndex, foldl1')

-- kde
import XMonad.Config.Kde

main = do
  xmonad $ docks kde4Config
    { manageHook = manageHook kdeConfig <+> myManageHook
    , layoutHook = smartBorders $ avoidStruts $
                   (smartSpacing 5 $ withBorder 2 $ Tall 1 (3/100) (1/2)) |||
                   (smartSpacing 5 $ withBorder 2 $ Mirror (Tall 1 (3/100) (1/2))) |||
                   Full |||
                   tabbed shrinkText def
    , startupHook = startup
    , modMask     = mod4Mask
    } `additionalKeys` myKeys `removeKeys` myRemoveKeys

myRemoveKeys =
  [ (mod4Mask, xK_Tab)
  , (mod4Mask .|. shiftMask, xK_Tab)
  ]

myManageHook = composeAll . concat $
  [ [ className   =? c --> doFloat           | c <- myFloats]
  , [ title       =? p --> doFloat           | p <- plasmaWindows]
  ]
  where myFloats      = ["Gimp"]
        plasmaWindows =
          [ "yakuake"
          , "Yakuake"
          , "Kmix"
          , "kmix"
          , "plasma"
          , "Plasma"
          , "plasma-desktop"
          , "Plasma-desktop"
          , "krunner"
          , "ksplashsimple"
          , "ksplashqml"
          , "ksplashx"
          ]

startupList :: [String]
startupList =
  [ "owncloud"
  , "compton"
  ]

startup :: X ()
startup = do
  foldl1' (>>) $ map (spawn . ifNotRunning) startupList

-- Wrap a command in Bash that checks if it's running.
ifNotRunning :: String -> String
ifNotRunning s = "if [ `pgrep -c " ++ (basename s) ++ "` == 0 ]; then " ++ s ++ "; fi"

-- Grab the program name from a command (everything up to the space,
-- if there's a space). Doesn't work with escaped spaces.
basename :: String -> String
basename s = case elemIndex ' ' s of
  (Just n) -> take n s
  Nothing -> s

myKeys = [
  -- extra programs
  ((mod4Mask, xK_x),
       spawn "emacsclient -c")
  , ((mod4Mask, xK_z),
       spawn "firefox-nightly")
  , ((mod4Mask, xK_m),
       spawn ":"
       -- TODO put social stuff here (Discord, Riot) and open it on a particular workspace
    )
  ]
