#!/bin/bash

# Read the input file into an array
map=()
while read -r line; do
    map+=("$line")
done < input.txt

rows=${#map[@]}
cols=${#map[0]}

# Function to check visibility of a tree
is_visible() {
    local x=$1 y=$2 height=${map[$y]:$x:1}
    for dir in {0..3}; do
        case $dir in
            0) step=1; dx=0; dy=-1;; # up
            1) step=1; dx=0; dy=1;;  # down
            2) step=1; dx=-1; dy=0;; # left
            3) step=1; dx=1; dy=0;;  # right
        esac
        for ((i=1; i<rows || i<cols; i++)); do
            nx=$((x + i * dx))
            ny=$((y + i * dy))
            if (( nx < 0 || ny < 0 || nx >= cols || ny >= rows )); then
                break
            fi
            if (( ${map[$ny]:$nx:1} >= height )); then
                break
            fi
            ((visible++))
        done
    done
}

# Count visible trees
visible=0
for ((y=0; y<rows; y++)); do
    for ((x=0; x<cols; x++)); do
        if ((x == 0 || y == 0 || x == cols - 1 || y == rows - 1)); then
            ((visible++))
        else
            is_visible $x $y && ((visible++))
        fi
    done
done

echo "Visible trees: $visible"

# Function to calculate scenic score
scenic_score() {
    local x=$1 y=$2 height=${map[$y]:$x:1} score=1
    for dir in {0..3}; do
        count=0
        case $dir in
            0) step=1; dx=0; dy=-1;; # up
            1) step=1; dx=0; dy=1;;  # down
            2) step=1; dx=-1; dy=0;; # left
            3) step=1; dx=1; dy=0;;  # right
        esac
        for ((i=1; i<rows || i<cols; i++)); do
            nx=$((x + i * dx))
            ny=$((y + i * dy))
            if (( nx < 0 || ny < 0 || nx >= cols || ny >= rows )); then
                break
            fi
            ((count++))
            if (( ${map[$ny]:$nx:1} >= height )); then
                break
            fi
        done
        ((score *= count))
    done
    echo $score
}

# Find the maximum scenic score
max_score=0
for ((y=1; y<rows-1; y++)); do
    for ((x=1; x<cols-1; x++)); do
        score=$(scenic_score $x $y)
        ((score > max_score)) && max_score=$score
    done
done

echo "Highest scenic score: $max_score"