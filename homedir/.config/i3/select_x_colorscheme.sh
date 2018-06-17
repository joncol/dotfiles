#!/usr/bin/env bash

declare -A schemes
scheme_path=~/code/dotfiles/palettes
scheme_names=()
while IFS= read -d $'\0' -r f; do
    name=$(basename "$f" .txt)
    schemes["$name"]="$f"
    scheme_names+="$name\n"
done < <(/usr/bin/find "$scheme_path" -maxdepth 1 -type f -print0)
sel_scheme=$(echo -e "$scheme_names" | /usr/bin/dmenu -i -p 'Select scheme')
echo "#define SELECTED_THEME $sel_scheme" > ~/.selected_theme.h
if [ -e "${schemes["$sel_scheme"]}" ]; then
    xrdb -m ${schemes["$sel_scheme"]}
    urxvt
fi
