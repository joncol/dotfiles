#!/bin/bash
sink_name=0
pactl set-sink-volume $sink_name +4% && pactl set-sink-mute $sink_name 0
