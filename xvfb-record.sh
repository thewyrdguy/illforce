#!/bin/sh

file="$1"
srv=44
recpid="/tmp/recpid"

bindir="`dirname $0`"

echo about to start xvfb-run
xvfb-run --server-num $srv --auth-file /tmp/xvfb.auth \
	-s "-ac -screen 0 1280x720x24" $bindir/playanimation $recpid "$file" &
playpid=$!
echo started xvfb-run, pid $playpid

dst=`echo $file|sed -e 's/\.txt//'`.avi
rm -f "$dst"
echo starting record into "$dst"
ffmpeg -draw_mouse 0 -f x11grab -video_size 1280x720 -i :$srv \
	"$dst" &
echo $! >$recpid
echo started recording, pid `cat $recpid`

#	-codec:v libx264 -r 29.97 "$dst"
#	-codec:v mpeg1video

echo waiting for record to complete (pid `cat $recpid`)
wait `cat $recpid`
echo record completed, waiting for play to complete (pid $playpid)
wait $playpid
echo play completed
