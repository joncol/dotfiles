#!/usr/bin/env bash

declare -A themes
theme_path=~/.config/alacritty/alacritty-theme/themes
theme_names=()
while IFS= read -d $'\0' -r f; do
    name=$(basename ${f%.*})
    themes["$name"]="$f"
    theme_names+="$name\n"
done < <(/usr/bin/find "$theme_path" -maxdepth 1 -type f -print0 | sort -z)
sel_theme=$(echo -e "$theme_names" | \
                dmenu -fn 'Montserrat-12:medium:antialias=true' \
                      -h 20 -dim 0.4 -y 2 -sf "#3f3f3f" -sb "#4bcffa" \
                      -i -p 'Select theme')
sel_theme_file="${themes["$sel_theme"]}"
if [ -e "$sel_theme_file" ]; then
    ~/.local/bin/alacritty-colorscheme \
        -C ~/.config/alacritty/alacritty-theme/themes \
        -a $(basename $sel_theme_file)
    notify-send "Theme selected: $sel_theme"
fi
