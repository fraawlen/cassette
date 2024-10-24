#!/bin/sh

ffmpeg \
	-i raw/$1.webm \
	-i frames/$1.png \
	-filter_complex "[0]crop=1600:800:(in_w-1600)/2:(in_h-800)/2[cropped];[cropped][1]overlay=x=(W-w)/2:y=(H-h)/2" \
	-loop 0 \
	results/$1.webp
