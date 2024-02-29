#!/bin/bash

# Read input from file "input.txt"
input=$(<input.txt)

# Initialize starting position and direction
x=0
y=0
dirIndex=0

# Define directions (North, East, South, West)
declare -a directions
directions[0]="0 1"
directions[1]="1 0"
directions[2]="0 -1"
directions[3]="-1 0"

# Split input instructions by ", "
IFS=', ' read -r -a instructions <<< "$input"

# Loop through each instruction
for instruction in "${instructions[@]}"; do
  turn=${instruction:0:1}
  blocks=${instruction:1}

  # Adjust direction based on turn
  if [ "$turn" == "R" ]; then
    ((dirIndex=(dirIndex+1)%4))
  else
    ((dirIndex=(dirIndex-1+4)%4))
  fi

  # Read direction deltas
  IFS=' ' read -r dx dy <<< "${directions[$dirIndex]}"

  # Move position based on direction and blocks
  ((x+=dx*blocks))
  ((y+=dy*blocks))
done

# Calculate and print Manhattan distance
echo $(( ${x#-} + ${y#-} ))