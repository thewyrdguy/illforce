#!/bin/sh

file="$1"

rm -f "$file"
ffmpeg -draw_mouse 0 -f x11grab -video_size 1280x720 -i :44 \
	-codec:v libx264 -r 29.97 "$file"
