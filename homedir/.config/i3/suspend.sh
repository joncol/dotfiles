#!/usr/bin/env sh

~/.local/bin/betterlockscreen --lock dim &
sleep 0.5 && systemctl suspend
