#!/usr/bin/env bash

monitor="DVI-D-0"

finger_id=$(xinput list | grep -iE 'Wacom Intuos Pro S Finger' | \
            cut -f2 | sed s/id=//)

if [[ -n $finger_id ]]; then
    echo "Disabling Wacom touch (device ID: $finger_id)"
    xinput disable $finger_id
fi

pen_id=$(xinput list | grep -iE 'Wacom Intuos Pro S Pen Pen' | \
        cut -f2 | sed s/id=//)

if [[ -n $pen_id ]]; then
    echo "Restricting Wacom pen (device ID: $pen_id) to monitor $monitor"
    xinput map-to-output $pen_id $monitor
fi

eraser_id=$(xinput list | grep -iE 'Wacom Intuos Pro S Pen Eraser' | \
            cut -f2 | sed s/id=//)

if [[ -n $eraser_id ]]; then
    echo "Restricting Wacom eraser (device ID: $pen_id) to monitor $monitor"
    xinput map-to-output $eraser_id $monitor
fi
