#!/usr/bin/env bash

if ! pgrep -x screenkey >/dev/null; then
    GEOM=$(slop -n -f '%g')
    screenkey -g $GEOM &
    notify-send "Starting screenkey"
else
    notify-send "Stopping screenkey"
    pkill screenkey
fi
