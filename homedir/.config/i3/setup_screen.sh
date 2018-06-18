#!/usr/bin/env bash

DIM=$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/')
W=$(echo $DIM | sed -r 's/^([0-9]+)x[0-9]+.*$/\1/')
H=$(echo $DIM | sed -r 's/^[0-9]+x([0-9]+).*$/\1/')
printf "#define WIDTH $W\n#define HEIGHT $H\n" > ~/.dimensions.h
xrdb -m ~/.Xresources

xrandr | grep -q "eDP-1 connected.*1920x1080"
if [ $? -eq 0 ]; then
    echo Found laptop screen
    xrandr --output eDP-1 --auto
fi

xrandr | grep -q "DP-1-1 connected.*3840x2160"
if [ $? -eq 0 ]; then
    echo Found work screen
    xrandr --output DP-1-1 --auto --right-of eDP-1
    xrandr --output eDP-1 --off
    exit 0
fi

xrandr | grep -q "^HDMI-2 connected "
if [ $? -eq 0 ]; then
    echo Found left home screen
    xrandr --output HDMI-2 --mode 1920x1200 --right-of eDP-1
    xrandr --output eDP-1 --off
fi

xrandr | grep -q "^DP-1-1 connected"
if [ $? -eq 0 ]; then
    echo Found right home screen
    xrandr --output DP-1-1 --mode 1920x1200 --right-of HDMI-2
fi
