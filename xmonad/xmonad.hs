import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.EZConfig(additionalKeys)

-- For shutdown commands and keys
import Data.Map
import XMonad.Prompt
import XMonad.Prompt.XMonad
import System.Exit

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
	, logHook = dynamicLogWithPP xmobarPP
		{ ppOutput = hPutStrLn xmproc
		, ppTitle = xmobarColor "green" "" . shorten 50
		}
        , startupHook = startup
        , terminal    = "gnome-terminal"
	, modMask     = mod4Mask
	} `additionalKeys`
        [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask, xK_p ), spawn "rofi -show run")
        , ((mod4Mask .|. shiftMask, xK_q),
           xmonadPrompt defaultXPConfig
           { promptKeymap = fromList
             [ ((0, xK_r), do
                   spawn "emacsclient -e '(kill emacs)'"
                   spawn "systemctl reboot")
             , ((shiftMask, xK_s), do
                   spawn "emacsclient -e '(kill emacs)'"
                   spawn "sudo poweroff")
             , ((0, xK_e), do
                   spawn "emacsclient -e '(kill emacs)'"
                   io $ exitWith ExitSuccess)
             , ((0, xK_l),  spawn "xscreensaver-command -lock")
             , ((0, xK_s), do
                   spawn "xscreensaver-command -lock"
                   spawn "systemctl suspend")
             , ((0, xK_Escape), quit)
             ]
           , defaultText = "(r) Reboot, (S) Shutdown, (e) Exit, (l) Lock, (s) Sleep"
           })
        ]

startup :: X ()
startup = do
  spawn "feh --bg-scale ~/Owncloud/Backgrounds/Xmbindings.png"
  spawn "owncloud"
  spawn "trayer --transparent true --alpha 0 --tint 0x00000000 --SetDockType true --expand true --edge top --align right --width 15 --height 18"
