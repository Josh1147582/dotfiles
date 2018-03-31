import System.Posix.Env (getEnv)
import Data.Maybe (maybe)
import Control.Monad(when)

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
import XMonad.Prompt.ConfirmPrompt
import System.Exit(ExitCode(ExitSuccess), exitWith)
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Util.Dmenu

-- Brightness and audio keys
import Graphics.X11.ExtraTypes.XF86

import Data.List(elemIndex, foldl1')
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- kde
import XMonad.Config.Kde

import XMonad.Hooks.EwmhDesktops

myModMask = mod4Mask
myTerminal   = "konsole"

main = do
  session <- do
    s <- getEnv "DESKTOP_SESSION"
    return (maybe "xmonad" id s)
  thisDesktopConfig <- case session of
    "xmonad" -> do
      xmproc <- spawnPipe "xmobar"
      return desktopConfig {
        logHook = dynamicLogWithPP xmobarPP {
          ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 50
          }}
    _ -> return kde4Config
  xmonad $ ewmh $ docks thisDesktopConfig {
    manageHook = manageDocks <+> manageHook thisDesktopConfig <+> myManageHook
  -- { manageHook = manageDocks <+> manageHook thisDesktopConfig <+> myManageHook
  , layoutHook = desktopLayoutModifiers $ smartBorders $ avoidStruts $
                 (smartSpacing 5 $ withBorder 2 $ Tall 1 (3/100) (1/2)) |||
                 (smartSpacing 5 $ withBorder 2 $ Mirror (Tall 1 (3/100) (1/2))) |||
                 -- Full |||

                 -- Tabs are bugged/don't work in ewmh. On the
                 -- bright side, it makes a window float over KDE's
                 -- bar, which is what I want fullscreen to do.

                 -- It's not a bug, it's a feature.
                 simpleTabbed

  , startupHook = if session == "xmonad" then startup (startupList ++ xmonadStartupList) else startup startupList
  , handleEventHook = handleEventHook def <+> fullscreenEventHook
  , modMask     = mod4Mask
  , keys        = \c -> mySetKeys session c `M.union` keys thisDesktopConfig c
  } --`additionalKeys` (if session == "xmonad" then (myKeys ++ xmonadKeys) else myKeys)
    `removeKeys` myRemoveKeys session

xmonadStartupList =
  [ "feh --bg-scale ~/Owncloud/Backgrounds/Xmbindings.png"
  , "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --alpha 0 --tint 0x000000 --height 22"
  , "pasystray"
  , "xfce4-clipman"
  , "xbacklight -set 12"
  , "compton"
  , "xscreensaver -nosplash"
  ]

confirm :: String -> X () -> X ()
confirm m f = do
  result <- dmenu [m]
  when (init result == m) f

mySetKeys session conf@(XConfig {XMonad.modMask = myModMask}) =
  if session == "xmonad" then
    M.fromList $ myKeys ++ xmonadKeys
  else
    M.fromList $ myKeys
  where
    xmonadKeys = [
  -- scrot
        ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0, xK_Print), spawn "scrot")

      -- rofi
      , ((myModMask, xK_p ), spawn "rofi -show run")
      -- shutdown
      , ((myModMask .|. shiftMask, xK_q), confirm "Exit" $ io (exitWith ExitSuccess))
      --, ((myModMask .|. shiftMask, xK_q),
      --   xmonadPrompt defaultXPConfig
      --   { promptKeymap = fromList
      --     [ ((0, xK_r), do
      --           spawn "emacsclient -e '(kill emacs)'"
      --           spawn "systemctl reboot")
      --     , ((0 , xK_s), do
      --           spawn "emacsclient -e '(kill emacs)'"
      --           spawn "sudo poweroff")
      --     , ((0, xK_e), do
      --           spawn "emacsclient -e '(kill emacs)'"
      --           io $ exitWith ExitSuccess)
      --     , ((0, xK_l),  do
      --           spawn "xscreensaver-command -lock"
      --           quit)
      --     , ((0, xK_z), do
      --           spawn "xscreensaver-command -lock"
      --           spawn "systemctl suspend"
      --           quit)
      --     , ((0, xK_Escape), quit)
      --     ]
      --   , defaultText = "(r) Reboot, (s) Shutdown, (e) Exit, (l) Lock, (z) Sleep"
      --   })
      -- pulseaudio
      , ((0, xF86XK_AudioRaiseVolume),
             spawn "pactl set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo +5%")
      , ((0, xF86XK_AudioLowerVolume),
             spawn "pactl set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo -5%")
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
      ]
    myKeys =
      [
      -- extra programs
      ((myModMask, xK_x),
           spawn "emacsclient -c")
      , ((myModMask, xK_z),
           spawn "firefox-nightly")
      , ((myModMask, xK_m),
           spawn ":"
           -- TODO put social stuff here (Discord, Riot) and open it on a particular workspace
        )

      -- defaults

      -- Spawn terminal.
      , ((myModMask .|. shiftMask, xK_Return),
         spawn myTerminal)

      -- Close focused window.
      , ((myModMask .|. shiftMask, xK_c),
         kill)

      -- Cycle through the available layout algorithms.
      , ((myModMask, xK_space),
         sendMessage NextLayout)

      --  Reset the layouts on the current workspace to default.
      , ((myModMask .|. shiftMask, xK_space),
         setLayout $ XMonad.layoutHook conf)

      -- Resize viewed windows to the correct size.
      , ((myModMask, xK_n),
         refresh)

      -- Move focus to the next window.
      , ((myModMask, xK_Tab),
         windows W.focusDown)

      -- Move focus to the next window.
      , ((myModMask, xK_j),
         windows W.focusDown)

      -- Move focus to the previous window.
      , ((myModMask, xK_k),
         windows W.focusUp  )

      -- Move focus to the master window.
      , ((myModMask, xK_m),
         windows W.focusMaster  )

      -- Swap the focused window and the master window.
      , ((myModMask, xK_Return),
         windows W.swapMaster)

      -- Swap the focused window with the next window.
      , ((myModMask .|. shiftMask, xK_j),
         windows W.swapDown  )

      -- Swap the focused window with the previous window.
      , ((myModMask .|. shiftMask, xK_k),
         windows W.swapUp    )

      -- Shrink the master area.
      , ((myModMask, xK_h),
         sendMessage Shrink)

      -- Expand the master area.
      , ((myModMask, xK_l),
         sendMessage Expand)

      -- Push window back into tiling.
      , ((myModMask, xK_t),
         withFocused $ windows . W.sink)

      -- Increment the number of windows in the master area.
      , ((myModMask, xK_comma),
         sendMessage (IncMasterN 1))

      -- Decrement the number of windows in the master area.
      , ((myModMask, xK_period),
         sendMessage (IncMasterN (-1)))

      -- Toggle the status bar gap.

      -- Restart xmonad.
      , ((myModMask, xK_q),
         restart "xmonad" True)
      ]
      ++

      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      [((m .|. myModMask, k), windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
      ++

      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
          | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myRemoveKeys s =
  [ (mod4Mask, xK_Tab)
  , (mod4Mask .|. shiftMask, xK_Tab)
  ]
  -- ++
  -- if s == "xmonad" then
  --   [(mod4Mask, xK_p)]
  -- else
  --   []

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
  [ "compton"
  , "nextcloud"
  ]

startup :: [String] -> X ()
startup l = do
  foldl1' (>>) $ map (spawn . ifNotRunning) l

-- Wrap a command in Bash that checks if it's running.
ifNotRunning :: String -> String
ifNotRunning s = "if [ `pgrep -c " ++ (basename s) ++ "` == 0 ]; then " ++ s ++ "; fi"

-- Grab the program name from a command (everything up to the space,
-- if there's a space). Doesn't work with escaped spaces.
basename :: String -> String
basename s = case elemIndex ' ' s of
  (Just n) -> take n s
  Nothing -> s

