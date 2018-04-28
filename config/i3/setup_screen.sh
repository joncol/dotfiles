#!/usr/bin/env bash

xrandr | grep "^DP1-1 connected "
if [ $? -eq 0 ]; then
    echo Found work screen
    xrandr --output DP1-1 --auto --right-of eDP1
    exit 0
fi

xrandr | grep "^DP1 connected "
if [ $? -eq 0 ]; then
    echo Found left home screen
    xrandr --output DP1 --auto --right-of eDP1
    xrandr --output eDP1 --off
fi

xrandr | grep "^HDMI2 connected "
if [ $? -eq 0 ]; then
    echo Found right home screen
    xrandr --output HDMI2 --auto --right-of DP1
fi
