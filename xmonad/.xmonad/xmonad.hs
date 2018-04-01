import XMonad
import XMonad.Hooks.DynamicLog(dynamicLogWithPP
                              , xmobarPP
                              , ppOutput
                              , ppLayout
                              , ppTitle)
import XMonad.Hooks.ManageDocks(docks, docksEventHook, manageDocks, avoidStruts)
import XMonad.Util.Run(spawnPipe, hPutStrLn, runProcessWithInput)

-- Layouts
import XMonad.Layout.Spacing(smartSpacing)
import XMonad.Layout.Tabbed(simpleTabbed)
import XMonad.Layout.NoBorders(withBorder, smartBorders)
import XMonad.Layout.IndependentScreens(countScreens)

-- Shutdown commands and keys
import Data.Map(fromList)
import XMonad.Util.EZConfig(removeKeys)

-- For starting up a list of programs
import Data.List(elemIndex, foldl1')

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Config.Kde(kde4Config, desktopLayoutModifiers)

import XMonad.Hooks.EwmhDesktops(ewmh, fullscreenEventHook)

myModMask = mod4Mask
myTerminal   = "konsole"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppTitle = \_ -> ""
                , ppLayout = \_ -> ""}

main = do
  nScreen <- countScreens
  xmprocs <- mapM (\dis -> spawnPipe ("xmobar -x " ++ show dis)) [0..nScreen-1]
  xmonad $ ewmh $ docks $ kde4Config {
    manageHook = manageDocks <+> myManageHook <+> manageHook kde4Config
  , layoutHook = avoidStruts $ desktopLayoutModifiers $ smartBorders $
                 (smartSpacing 5 $ withBorder 2 $ Tall 1 (3/100) (1/2)) |||
                 (smartSpacing 5 $ withBorder 2 $ Mirror (Tall 1 (3/100) (1/2))) |||
                 -- Full |||

                 -- Tabs are bugged/don't work in ewmh. On the
                 -- bright side, it makes a window float over KDE's
                 -- bar, which is what I want fullscreen to do.

                 -- It's not a bug, it's a feature.
                 simpleTabbed

  , logHook = dynamicLogWithPP myPP {
      ppOutput = \s -> sequence_ [hPutStrLn h s | h <- xmprocs]
    }
  , startupHook = startup startupList
  , handleEventHook = handleEventHook kde4Config <+> fullscreenEventHook <+> docksEventHook
  , modMask     = mod4Mask
  , keys        = \c -> myKeys c `M.union` keys kde4Config c
  }
    `removeKeys` myRemoveKeys

myKeys conf@(XConfig {XMonad.modMask = myModMask}) = M.fromList $
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

myRemoveKeys =
  [ (mod4Mask, xK_Tab)
  , (mod4Mask .|. shiftMask, xK_Tab)
  ]
  ++
  -- if s == "xmonad" then
    [(mod4Mask, xK_p)]
  -- else
  --  []

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
  -- TODO find a way around this dirty hack
  , "sleep 5 && for i in `xdotool search --all --name xmobar`; do xdotool windowraise $i; done"
  ]

startup :: [String] -> X ()
startup l = do
  foldl1' (>>) $ map (spawn . ifNotRunning) l

-- Wrap a command in Bash that checks if it's running.
-- TODO do this in haskell
ifNotRunning :: String -> String
ifNotRunning s = "if [ `pgrep -c " ++ (basename s) ++ "` == 0 ]; then " ++ s ++ "; fi"

-- Grab the program name from a command (everything up to the space,
-- if there's a space). Doesn't work with escaped spaces.
basename :: String -> String
basename s = case elemIndex ' ' s of
  (Just n) -> take n s
  Nothing -> s

