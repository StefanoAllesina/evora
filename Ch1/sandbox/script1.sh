#!/bin/bash
# the previousnot a comment: where to find the bash program

cat $1 | sort | uniq > nodup_$1

