#!/usr/bin/env sh

SOURCE=`pactl list short sources | grep 'alsa_input\.pci' | cut -f1`
echo "Enabling microphone (source: ${SOURCE})"
pacmd set-default-source ${SOURCE}
