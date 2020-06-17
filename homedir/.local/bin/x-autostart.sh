#!/usr/bin/env bash

~/.local/bin/setup_mouse.sh
~/.local/bin/fix_nvidia_tearing.sh
~/.local/bin/setup_screen.pl
~/.local/bin/apply_xrdb_settings.sh

picom --unredir-if-possible --experimental-backends -b
~/.local/bin/launch.sh redshift &

xset r rate 300 30

~/.local/bin/launch.sh xcape -t 150 -e 'Control_L=BackSpace'

feh --bg-fill /usr/share/backgrounds/gnome/SeaSunset.jpg

~/.local/bin/launch.sh unclutter &
