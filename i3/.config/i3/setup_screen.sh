#!/usr/bin/env bash

DIM=$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/')
W=$(echo $DIM | sed -r 's/^([0-9]+)x[0-9]+.*$/\1/')
H=$(echo $DIM | sed -r 's/^[0-9]+x([0-9]+).*$/\1/')
printf "#define WIDTH $W\n#define HEIGHT $H\n" > ~/.dimensions.h
xrdb ~/.Xresources

xrandr | grep -zoq -A1 "DP1-1 connected.*3840x2160"
if [ $? -eq 0 ]; then
    echo Found work screen
    xrandr --output DP1-1 --auto --right-of eDP1
    exit 0
fi

xrandr | grep -q "^HDMI2 connected "
if [ $? -eq 0 ]; then
    echo Found left home screen
    xrandr --output HDMI2 --mode 1920x1200 --right-of eDP1
    xrandr --output eDP1 --off
fi

xrandr | grep -q "^DP1-1 connected"
if [ $? -eq 0 ]; then
    echo Found right home screen
    xrandr --output DP1-1 --mode 1920x1200 --right-of HDMI2
fi
