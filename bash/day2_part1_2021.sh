#!/bin/bash

# Initialize variables
horizontalPosition=0
depth=0

# Read from input.txt
while IFS=' ' read -r direction units || [[ -n "$direction" ]]; do
    case $direction in
        "forward")
            horizontalPosition=$((horizontalPosition + units))
            ;;
        "down")
            depth=$((depth + units))
            ;;
        "up")
            depth=$((depth - units))
            ;;
    esac
done < "input.txt"

# Calculate product
product=$((horizontalPosition * depth))

# Print the result
echo $product