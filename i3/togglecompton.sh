#!/bin/bash

if pidof compton
then
    echo Killing compton
    pkill compton
else
    echo Starting compton
    compton -bcG
fi
