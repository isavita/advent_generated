#!/bin/bash

# Read the list of instructions from input.txt
input=$(<input.txt)

# Starting position (0,0) facing North
x=0
y=0
dirIndex=0

# Direction vectors: North, East, South, West
declare -a directions

directions[0]="0 1"
directions[1]="1 0"
directions[2]="0 -1"
directions[3]="-1 0"

# Split the instructions by comma and space
IFS=", " read -r -a instructions <<< "$input"

# Keep track of visited coordinates
declare -A visited
visited["0,0"]=1

for instruction in "${instructions[@]}"; do
    turn=${instruction:0:1}
    blocks=${instruction:1}

    # Update direction index
    if [[ $turn == "R" ]]; then
        ((dirIndex=(dirIndex+1)%4))
    else
        ((dirIndex=(dirIndex-1+4)%4))
    fi

    # Current movement deltas
    IFS=' ' read -r dx dy <<< "${directions[$dirIndex]}"

    # Move one block at a time to track visited positions
    for ((step=0; step<blocks; step++)); do
        ((x+=dx))
        ((y+=dy))
        coord="$x,$y"
        if [[ -n ${visited[$coord]} ]]; then
            echo $(( ${x#-} + ${y#-} ))
            exit 0
        fi
        visited[$coord]=1
    done
done

exit 0
