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
import XMonad.Util.EZConfig(additionalKeys)

-- Brightness and audio keys
import Graphics.X11.ExtraTypes.XF86

import Data.List(elemIndex)

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = smartBorders $ avoidStruts $
                   (smartSpacing 5 $ withBorder 2 $ Tall 1 (3/100) (1/2)) |||
                   (smartSpacing 5 $ withBorder 2 $ Mirror (Tall 1 (3/100) (1/2))) |||
                   Full |||
                   tabbed shrinkText def
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "green" "" . shorten 50
      }
    , startupHook = startup
    , terminal    = "gnome-terminal"
    , modMask     = mod4Mask
    } `additionalKeys` myKeys

startup :: X ()
startup = do
  spawn "feh --bg-scale ~/Owncloud/Backgrounds/Xmbindings.png"
  spawn $ ifNotRunning "owncloud"
  spawn $ ifNotRunning "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --alpha 0 --tint 0x000000 --height 22"
  -- spawn "trayer --transparent true --alpha 0 --tint 0x00000000 --SetDockType true --expand true --edge top --align right --width 15 --height 18"
  -- spawn "stalonetray -geometry 1x1+1900 -bg '#000000' --kludges use_icons_hints --grow-gravity NE --icon-gravity NE -i 20 -t false"
  spawn $ ifNotRunning "pasystray"
  spawn $ ifNotRunning "xfce4-clipman"
  spawn "xbacklight -set 12"
  spawn $ ifNotRunning "compton"
  spawn $ ifNotRunning "xscreensaver -nosplash"

-- Wrap a command in Bash that checks if it's running.
ifNotRunning :: String -> String
ifNotRunning s = "if ! [ `pgrep " ++ (basename s) ++ "` ]; then " ++ s ++ "; fi"

-- Grab the program name from a command (everything up to the space,
-- if there's a space). Doesn't work with escaped spaces.
basename :: String -> String
basename s = case elemIndex ' ' s of
  (Just n) -> take n s
  Nothing -> s

myKeys = [
  -- scrot
  ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
  , ((0, xK_Print), spawn "scrot")

  -- rofi
  , ((mod4Mask, xK_p ), spawn "rofi -show run")

  -- shutdown
  , ((mod4Mask .|. shiftMask, xK_q),
     xmonadPrompt defaultXPConfig
     { promptKeymap = fromList
       [ ((0, xK_r), do
             spawn "emacsclient -e '(kill emacs)'"
             spawn "systemctl reboot")
       , ((0 , xK_s), do
             spawn "emacsclient -e '(kill emacs)'"
             spawn "sudo poweroff")
       , ((0, xK_e), do
             spawn "emacsclient -e '(kill emacs)'"
             io $ exitWith ExitSuccess)
       , ((0, xK_l),  do
             spawn "xscreensaver-command -lock"
             quit)
       , ((0, xK_z), do
             spawn "xscreensaver-command -lock"
             spawn "systemctl suspend"
             quit)
       , ((0, xK_Escape), quit)
       ]
     , defaultText = "(r) Reboot, (s) Shutdown, (e) Exit, (l) Lock, (z) Sleep"
     })
  -- pulseaudio
  , ((0, xF86XK_AudioRaiseVolume),
         spawn "pactl set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo +5%")
  , ((0, xF86XK_AudioLowerVolume),
         spawn "pactl set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo -XF86AudioMute exec --no-startup-id pactl set-sink-mute alsa_output.pci-0000_00_1f.3.analog-stereo toggle5%")
  , ((0, xF86XK_AudioMute),
         spawn "pactl set-sink-mute alsa_output.pci-0000_00_1f.3.analog-stereo toggle")

  -- brightness
  , ((0, xF86XK_MonBrightnessUp),
     let
        returnValM = fmap init $ runProcessWithInput "xbacklight" [] ""
     in do
       currentBrightness <- returnValM
       if (read currentBrightness :: Double) == 0 then
         spawn "xbacklight -set 2"
       else
         spawn "xbacklight -inc 5")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
     -- Another option for brightness configuration: never let it reach 0.
     {-|
     let
        returnValM = fmap init $ runProcessWithInput "xbacklight" [] ""
     in do
       currentBrightness <- returnValM
       if (read currentBrightness :: Double) - 5 >= 0 then
         spawn "xbacklight -dec 5"
       else return ())
     -}

  -- extra programs
  , ((mod4Mask, xK_x),
       spawn "emacsclient -c")
  , ((mod4Mask, xK_z),
       spawn "firefox-nightly")
  , ((mod4Mask, xK_m),
       spawn ":"
       -- TODO put social stuff here (Discord, Riot) and open it on a particular workspace
    )

  ]
