#!/bin/bash

numbers=($(cat input.txt))

function parseTree {
    childCount=${numbers[$1]}
    metaCount=${numbers[$1+1]}
    index=$(( $1 + 2 ))
    
    sum=0
    for ((i=0; i<$childCount; i++)); do
        res=($(parseTree $index))
        sum=$(( $sum + ${res[0]} ))
        index=${res[1]}
    done
    
    for ((i=0; i<$metaCount; i++)); do
        sum=$(( $sum + ${numbers[$index+$i]} ))
    done
    index=$(( $index + $metaCount ))
    
    echo "$sum $index"
}

res=($(parseTree 0))
echo ${res[0]}