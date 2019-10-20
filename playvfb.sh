#!/bin/sh

file="$1"

echo "Press Enter to start playback"
read x
./animate <"$file"
echo "Press Enter to finish"
read x
