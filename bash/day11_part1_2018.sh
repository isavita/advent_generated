#!/bin/bash

# Read the grid serial number from the input file
serial_number=$(cat input.txt)

# Initialize the maximum power and its coordinates
max_power=-999999
max_x=0
max_y=0

# Iterate over all possible 3x3 squares in the grid
for ((x=1; x<=298; x++)); do
  for ((y=1; y<=298; y++)); do
    # Calculate the power of the current 3x3 square
    power=0
    for ((i=0; i<3; i++)); do
      for ((j=0; j<3; j++)); do
        rack_id=$((x+i+10))
        power_level=$((rack_id*(y+j)+serial_number))
        power_level=$((power_level*rack_id))
        power_level=$((power_level/100%10))
        power_level=$((power_level-5))
        ((power+=power_level))
      done
    done
    
    # Update the maximum power and its coordinates if necessary
    if ((power>max_power)); then
      max_power=$power
      max_x=$x
      max_y=$y
    fi
  done
done

# Print the coordinates of the top-left fuel cell of the 3x3 square with the largest total power
echo "$max_x,$max_y"