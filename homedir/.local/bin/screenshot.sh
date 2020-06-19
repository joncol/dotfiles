#!/usr/bin/env sh

FILENAME=$(dmenu -noinput -p "Screenshot filename:" \
                 -fn 'Montserrat-12:medium:antialias=true' \
                 -h 20 -y 2 -sf "#eeeeee")
maim -os ${FILENAME}
