#!/bin/bash

# Step 1: Read Input
input_file="input.txt"

# Step 2: Initialize Variables
score=0
depth=0
in_garbage=false
cancel_next=false
garbage_count=0

# Step 3: Process Stream
while IFS= read -r -n1 char || [[ -n "$char" ]]; do
    if $cancel_next; then
        cancel_next=false
        continue
    fi

    if $in_garbage; then
        if [[ "$char" == "!" ]]; then
            cancel_next=true
        elif [[ "$char" == ">" ]]; then
            in_garbage=false
        else
            ((garbage_count++))
        fi
    else
        case "$char" in
            "{") ((depth++)) ;;
            "}") ((score+=$depth)) && ((depth--)) ;;
            "<") in_garbage=true ;;
        esac
    fi
done < "$input_file"

# Step 4: Print Results
echo $garbage_count