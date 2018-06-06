#!/bin/bash
sink_name=alsa_output.pci-0000_00_1f.3.analog-stereo
pactl set-sink-mute $sink_name toggle