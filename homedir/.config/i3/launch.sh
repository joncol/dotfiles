#!/usr/bin/env bash

app="$1"
killall -q "$app"
while pgrep -u $UID -x "$app" >/dev/null; do sleep 1; done

"$@"
