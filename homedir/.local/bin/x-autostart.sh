#!/usr/bin/env bash

~/.local/bin/setup-mouse.sh
~/.local/bin/setup_screen.pl
# ~/.local/bin/apply-xrdb-settings.sh
~/.local/bin/setup-wacom.sh

# picom --unredir-if-possible --experimental-backends -b
~/.local/bin/launch.sh redshift &

# ~/.local/bin/launch.sh xcape -t 150 -e 'Control_L=BackSpace'

wallpaper_dir=~/Pictures/wallpapers
ls $wallpaper_dir | sort -R | tail -1 | while read file; do
    feh --bg-fill ${wallpaper_dir}/${file}
done

~/.local/bin/launch.sh unclutter &
