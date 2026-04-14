#!/usr/bin/env bash
#
# Unhide the mouse cursor when a new window opens.
# Listens to niri's event stream, tracks known window IDs,
# and nudges the mouse (via ydotool) when a new ID appears.

declare -A known_windows

# Seed known windows from the current window list.
while IFS= read -r line; do
    if [[ $line =~ id:\ ([0-9]+) ]]; then
        known_windows[${BASH_REMATCH[1]}]=1
    fi
done < <(niri msg windows 2>/dev/null)

niri msg event-stream 2>/dev/null | while IFS= read -r line; do
    if [[ $line == "Window opened or changed:"* ]]; then
        if [[ $line =~ id:\ ([0-9]+) ]]; then
            wid="${BASH_REMATCH[1]}"
            if [[ -z "${known_windows[$wid]}" ]]; then
                known_windows[$wid]=1
                # ydotool mousemove -x 1 -y 0
                ydotool mousemove -x -1 -y 0
            fi
        fi
    elif [[ $line == "Window closed:"* ]]; then
        if [[ $line =~ id:\ ([0-9]+) ]]; then
            unset 'known_windows[${BASH_REMATCH[1]}]'
        fi
    fi
done
