#!/bin/bash
sink_name=alsa_output.pci-0000_00_1f.3.analog-stereo
pactl set-sink-volume $sink_name -4% && pactl set-sink-mute $sink_name 0
