#!/bin/sh
xrandr \
    --output DP-0 --off \
    --output DP-1 --off \
    --output HDMI-0 --mode 1920x1080 --pos 3640x0 --rotate left \
    --output DP-2 --off \
    --output DP-3 --mode 1920x1080 --pos 0x0 --rotate right \
    --output DP-4 --mode 2560x1440 --pos 1080x240 --rotate normal --rate 120 \
    --output DP-5 --off --output USB-C-0 --off

feh --no-xinerama --bg-scale ~/Nextcloud/Backgrounds/garlic-space-green.jpg
