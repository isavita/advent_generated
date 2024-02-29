#!/bin/bash

earliestDeparture=$(head -n 1 input.txt)
busIDs=$(tail -n 1 input.txt | tr ',' '\n')

earliestBusID=0
minWaitTime=$earliestDeparture

for id in $busIDs; do
    if [ "$id" == "x" ]; then
        continue
    fi
    waitTime=$(( $id - ($earliestDeparture % $id) ))
    if [ $waitTime -lt $minWaitTime ]; then
        minWaitTime=$waitTime
        earliestBusID=$id
    fi
done

echo $(( $earliestBusID * $minWaitTime ))