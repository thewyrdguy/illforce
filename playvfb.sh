#!/bin/sh

file="$1"

echo "Press Enter to start playback"
read x
./animate -f <"$file"
echo "Press Enter to finish"
read x
