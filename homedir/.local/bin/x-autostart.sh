#!/usr/bin/env bash

~/.local/bin/setup-mouse.sh
~/.local/bin/setup_screen.pl
# ~/.local/bin/apply-xrdb-settings.sh
~/.local/bin/setup-wacom.sh

# picom --unredir-if-possible --experimental-backends -b
~/.local/bin/launch.sh redshift &

# ~/.local/bin/launch.sh xcape -t 150 -e 'Control_L=BackSpace'

currenttime=$(date +%H:%M)
if [[ "$currenttime" > "21:00" ]] || [[ "$currenttime" < "18:00" ]]; then
    feh --bg-fill ~/Pictures/wallpapers/dark.png
else
    feh --bg-fill ~/Pictures/wallpapers/light.jpg
fi

~/.local/bin/launch.sh unclutter &
