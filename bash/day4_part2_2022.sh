#!/bin/bash

# Initialize counter
count=0

# Process each line of the input file
while IFS= read -r line || [[ -n "$line" ]]; do
    # Split line into two parts based on comma
    IFS=',' read -r part1 part2 <<< "$line"
    
    # Extract ranges
    IFS='-' read -r start1 end1 <<< "$part1"
    IFS='-' read -r start2 end2 <<< "$part2"
    
    # Check if ranges overlap
    if [ "$start1" -le "$end2" ] && [ "$end1" -ge "$start2" ]; then
        count=$((count + 1))
    fi
done < "input.txt"

# Print the answer
echo "$count"