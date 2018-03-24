{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

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

import XMonad.Hooks.EwmhDesktops

import Control.Exception

import "DBus" DBus
import "DBus" DBus.Connection as DC
import "DBus" DBus.Message

import qualified Codec.Binary.UTF8.String as UTF8

prettyPrinter :: DC.Connection -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppCurrent  = wrap "[" "]"
    , ppSep      = " | "
    }

getWellKnownName :: DC.Connection -> IO ()
getWellKnownName dbus = tryGetName `catch` (\(DBus.Error _ _) -> getWellKnownName dbus)
  where
    tryGetName = do
        namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
        addArgs namereq [String "org.xmonad.Log", Word32 5]
        sendWithReplyAndBlock dbus namereq 0
        return ()

dbusOutput :: DC.Connection -> String -> IO ()
dbusOutput dbus str = do
    msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
    addArgs msg [String (UTF8.decodeString str)]
    -- If the send fails, ignore it.
    send dbus msg 0 `catch` (\(DBus.Error _ _) -> return 0)
    return ()

main :: IO ()
-- main = do
main = withConnection Session $ \dbus -> do
  getWellKnownName dbus
  xmonad $ ewmh $ docks kde4Config
    { manageHook = manageHook kdeConfig <+> myManageHook
    , logHook = dynamicLogWithPP (prettyPrinter dbus)
    , layoutHook = smartBorders $ avoidStruts $
                   (smartSpacing 5 $ withBorder 2 $ Tall 1 (3/100) (1/2)) |||
                   (smartSpacing 5 $ withBorder 2 $ Mirror (Tall 1 (3/100) (1/2))) |||
                   -- Full |||

                   -- Tabs are bugged/don't work in ewmh. On the
                   -- bright side, it makes a window float over KDE's
                   -- bar, which is what I want fullscreen to do.

                   -- It's not a bug, it's a feature.
                   simpleTabbed

    , startupHook = startup
    , handleEventHook = handleEventHook def <+> fullscreenEventHook
    , modMask     = mod4Mask
    } `additionalKeys` myKeys `removeKeys` myRemoveKeys

myRemoveKeys =
  [ (mod4Mask, xK_Tab)
  , (mod4Mask .|. shiftMask, xK_Tab)
  , (mod4Mask, xK_p)
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
  [ "compton"
  , "owncloud"
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
