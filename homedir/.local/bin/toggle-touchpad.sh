#!/bin/bash
touchpad=$(xinput list | grep -i "synaptics" | cut -f2 | sed s/id=//)
if [[ ! -z "${touchpad// }" ]]; then
    device_enabled_pid=$(xinput list-props $touchpad | grep -i "device enabled" | sed "s/[^(]*(\(.*\)).*/\1/")
    device_enabled=$(xinput list-props $touchpad | grep -i "device enabled" | cut -f3)
    echo "Touchpad device ID: $touchpad"
    if [ $device_enabled -eq 1 ]; then
        echo "Disabling touchpad"
        xinput set-prop $touchpad $device_enabled_pid 0
    else
        echo "Enabling touchpad"
        xinput set-prop $touchpad $device_enabled_pid 1
    fi
fi
