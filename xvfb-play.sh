#!/bin/sh

file="$1"

xvfb-run --server-num 44 --auth-file /tmp/xvfb.auth \
	-s "-ac -screen 0 1280x720x24" ./playvfb.sh "$file"

