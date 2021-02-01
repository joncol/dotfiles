#!/usr/bin/env bash

~/.local/bin/setup-mouse.sh
~/.local/bin/setup_screen.pl
# ~/.local/bin/apply-xrdb-settings.sh
~/.local/bin/setup-wacom.sh

# picom --unredir-if-possible --experimental-backends -b
~/.local/bin/launch.sh redshift &

xset r rate 300 30

# ~/.local/bin/launch.sh xcape -t 150 -e 'Control_L=BackSpace'

currenttime=$(date +%H:%M)
if [[ "$currenttime" > "23:00" ]] || [[ "$currenttime" < "11:30" ]]; then
    feh --bg-fill ~/Pictures/wallpapers/dark.png
else
    feh --bg-fill ~/Pictures/wallpapers/light.jpg
fi

~/.local/bin/launch.sh unclutter &
