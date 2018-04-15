#!/bin/bash
mouse=$(xinput list | grep -iE "\⎜.*mouse" | tail -1 | cut -f2 | sed s/id=//)

mouse_nat_scroll=$(xinput list-props $mouse | grep -i "natural scrolling enabled (" | sed "s/[^(]*(\(.*\)).*/\1/")
echo "Mouse device: $mouse"
echo "  Natural scrolling property ID: $mouse_nat_scroll"
xinput set-prop $mouse $mouse_nat_scroll 1

touchpad=$(xinput list | grep -i "synaptics touchpad" | cut -f2 | sed s/id=//)
if [[ ! -z "${touchpad// }" ]]; then
    tapping_enabled=$(xinput list-props $touchpad | grep -i "tapping enabled (" | sed "s/[^(]*(\(.*\)).*/\1/")
    touchpad_nat_scroll=$(xinput list-props $touchpad | grep -i "natural scrolling enabled (" | sed "s/[^(]*(\(.*\)).*/\1/")
    echo "Touchpad device ID: $touchpad"
    echo "  Tapping enabled property ID: $tapping_enabled"
    echo "  Natural scrolling property ID: $touchpad_nat_scroll"
    xinput set-prop $touchpad $tapping_enabled 1
    xinput set-prop $touchpad $touchpad_nat_scroll 1
fi
