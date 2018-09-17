#!/bin/bash

if pidof compton
then
    echo Killing compton
    pkill compton
else
    echo Starting compton
    compton -b --config ~/.i3/compton.conf
fi
