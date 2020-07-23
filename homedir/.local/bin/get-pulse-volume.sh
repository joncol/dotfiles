#!/usr/bin/env bash

muted=$(pacmd list-sinks | awk '/muted/ { print $2 }' | \
    head -n $(( $SINK + 1 )) | tail -n 1)

if [[ $muted == "yes" ]]; then
    echo "muted"
else
    pactl list sinks | grep '^[[:space:]]Volume:' | \
        head -n $(( $SINK + 1 )) | tail -n 1 | \
        sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'
fi
