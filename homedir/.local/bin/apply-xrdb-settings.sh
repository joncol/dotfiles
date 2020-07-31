#!/usr/bin/env sh

DIM=$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/')
W=$(echo $DIM | sed -r 's/^([0-9]+)x[0-9]+.*$/\1/')
H=$(echo $DIM | sed -r 's/^[0-9]+x([0-9]+).*$/\1/')
printf "#define WIDTH $W\n#define HEIGHT $H\n" > ~/.dimensions.h

xrdb ~/.Xresources 2> /dev/null

if [ -f ~/.xcolorscheme ]; then
    filename=$(cat ~/.xcolorscheme)
    tmp_file=$(mktemp "/tmp/palette.txt.XXX")
    cp "$filename" $tmp_file
    xrdb -m $tmp_file
    rm $tmp_file
fi
