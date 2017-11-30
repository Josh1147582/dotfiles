#!/bin/bash

if pidof compton
then
    echo Killing compton
    pkill compton
else
    echo Starting compton
    compton -bcG  --backend glx --vsync opengl-swc
fi
