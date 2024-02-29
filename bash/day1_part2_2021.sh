#!/bin/bash

# Read input from input.txt
input="input.txt"

prev=()
count=0
while IFS= read -r line || [[ -n "$line" ]]; do
    # Skip empty lines
    if [ -z "$line" ]; then
        continue
    fi

    # Push the new value into the array
    prev+=("$line")

    # Once we have 4 elements, start comparing
    if [ ${#prev[@]} -eq 4 ]; then
        prev_sum=$(( prev[0] + prev[1] + prev[2] ))
        curr_sum=$(( prev[1] + prev[2] + prev[3] ))
        if [ $curr_sum -gt $prev_sum ]; then
            count=$((count+1))
        fi
        
        # Remove the first element
        prev=("${prev[@]:1}")
    fi
done < "$input"

echo $count