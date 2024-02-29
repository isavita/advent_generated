#!/bin/bash

input="input.txt"
totalRiskLevel=0

# Read file line by line
while IFS= read -r line || [[ -n "$line" ]]; do
    heightmap+=("$line")
done < "$input"

# Get dimensions
height=${#heightmap[@]}
width=${#heightmap[0]}

# Helper function to compare heights
is_lower_than_neighbors() {
    local y=$1 x=$2
    local current_height=${heightmap[$y]:$x:1}
    # Left
    if [ $x -gt 0 ]; then
        if [ "${heightmap[$y]:$((x-1)):1}" -le "$current_height" ]; then return 1; fi
    fi
    # Right
    if [ $x -lt $((width-1)) ]; then
        if [ "${heightmap[$y]:$((x+1)):1}" -le "$current_height" ]; then return 1; fi
    fi
    # Up
    if [ $y -gt 0 ]; then
        if [ "${heightmap[$((y-1))]:$x:1}" -le "$current_height" ]; then return 1; fi
    fi
    # Down
    if [ $y -lt $((height-1)) ]; then
        if [ "${heightmap[$((y+1))]:$x:1}" -le "$current_height" ]; then return 1; fi
    fi
    return 0
}

# Calculate total risk level
for ((y=0; y<height; y++)); do
    for ((x=0; x<width; x++)); do
        if is_lower_than_neighbors $y $x; then
            (( totalRiskLevel+=1+${heightmap[$y]:$x:1} ))
        fi
    done
done

echo $totalRiskLevel