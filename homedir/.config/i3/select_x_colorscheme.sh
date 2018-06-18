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
sel_scheme_file="${schemes["$sel_scheme"]}"
if [ -e "$sel_scheme_file" ]; then
    echo "$sel_scheme_file" > ~/.xcolorscheme
    tmp_file=$(mktemp "/tmp/palette.txt.XXX")
    cp "$sel_scheme_file" $tmp_file
    xrdb -m $tmp_file
    rm $tmp_file
    urxvt -e $SHELL -c "neofetch;$SHELL -i"
fi
