#!/bin/bash
# launch R code several times, 
# each time using a different seed for the random number generator

for i in `seq 1 $2`;
  do
    echo "Launching program $1 with seed $i"
    Rscript $1 $i
  done

