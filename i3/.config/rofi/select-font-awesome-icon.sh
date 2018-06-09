#!/usr/bin/env bash

cd "$(dirname "$0")"

options='-columns 5 -width 80 -location 0 -lines 20 -bw 2 -yoffset -2'
selected=$(cat icon-list.txt | rofi -dmenu -i -markup-rows ${options} \
                                        -p "Select icon")

# exit if nothing is selected
[[ -z $selected ]] && exit

echo -ne $(echo "$selected" | \
               awk -F';' -v RS='>' '
    NR==2{sub("&#x","",$1);print "\\u" $1;exit}') | xclip -selection clipboard
