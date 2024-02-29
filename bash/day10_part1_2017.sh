
#!/bin/bash

# Read input lengths from a file
lengths=$(cat input.txt | tr ',' ' ')

# Initialize variables
list=($(seq 0 255))
currentPosition=0
skipSize=0

# Perform the knot-tying operations
for length in $lengths; do
    # Reverse the elements
    for ((i = 0; i < length / 2; i++)); do
        start=$(( (currentPosition + i) % 256 ))
        end=$(( (currentPosition + length - 1 - i) % 256 ))
        tmp=${list[$start]}
        list[$start]=${list[$end]}
        list[$end]=$tmp
    done

    # Move the current position and increase the skip size
    currentPosition=$(( (currentPosition + length + skipSize) % 256 ))
    ((skipSize++))
done

# Multiply the first two numbers in the list
result=$(( ${list[0]} * ${list[1]} ))
echo $result
