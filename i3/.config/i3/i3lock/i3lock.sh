#!/usr/bin/env bash

### Regular Splash Screen
#set -eu
#
#[[ -z "$(pgrep i3lock)" ]] || exit
#i3lock -n -u -t -i ${HOME}/.config/i3lock/lock.png

### Faded lockscreen from https://gist.github.com/csivanich/10914698
# i3lock blurred screen inspired by /u/patopop007 and the blog post
# http://plankenau.com/blog/post-10/gaussianlock

[[ -z "$(pgrep i3lock)" ]] || exit

IMAGE=/tmp/i3lock.png
SCREENSHOTCMD="scrot $IMAGE" # 0.46s

# Alternate screenshot method with imagemagick. NOTE: it is much slower
# SCREENSHOTCMD="import -window root $IMAGE" # 1.35s

# All options are here: http://www.imagemagick.org/Usage/blur/#blur_args
#BLURTYPE="0x5" # 7.52s
##BLURTYPE="0x2" # 4.39s
#BLURTYPE="5x2" # 3.80s
#BLURTYPE="2x8" # 2.90s
#BLURTYPE="2x3" # 2.92s

# Get the screenshot, add the blur and lock the screen with it
$SCREENSHOTCMD
#convert -channel RGB,Sync -radial-blur 4 $IMAGE $IMAGE
#convert $IMAGE -blur $BLURTYPE $IMAGE
convert -channel RGB,Sync -rotational-blur 2 $IMAGE $IMAGE
composite $HOME/.config/i3/i3lock/lockclip.png $IMAGE -gravity center $IMAGE
i3lock -i $IMAGE
rm $IMAGE
