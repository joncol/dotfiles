#!/usr/bin/env bash

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
