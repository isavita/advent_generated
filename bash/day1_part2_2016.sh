#!/bin/bash

# Path to the input file (same directory as this script)
script_dir="$(dirname "$0")"
input=$(<"$script_dir/input.txt")

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

# Keep track of visited coordinates without associative arrays for
# better portability across Bash versions.  Use a newline-delimited
# string to record every location we have seen.
visited="0,0"

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
        # Check if this coordinate was visited before
        if printf '%s\n' "$visited" | grep -Fxq -- "$coord"; then
            echo $(( ${x#-} + ${y#-} ))
            exit 0
        fi
        visited+=$'\n'$coord
    done
done

exit 0
