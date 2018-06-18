#!/usr/bin/env sh

xrdb ~/.Xresources 2> /dev/null

if [ -f ~/.xcolorscheme ]; then
    filename=$(cat ~/.xcolorscheme)
    tmp_file=$(mktemp "/tmp/palette.txt.XXX")
    cp "$filename" $tmp_file
    xrdb -m $tmp_file
    rm $tmp_file
fi
