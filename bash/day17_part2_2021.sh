#!/bin/bash

# Read target area from input.txt
read -r line < input.txt
[[ $line =~ x=([0-9]+)..([0-9]+),\ y=([-0-9]+)..([-0-9]+) ]]
x_min=${BASH_REMATCH[1]}
x_max=${BASH_REMATCH[2]}
y_min=${BASH_REMATCH[3]}
y_max=${BASH_REMATCH[4]}

max_height=0
valid_velocities=()

for x_vel in $(seq 0 $x_max); do
  for y_vel in $(seq $y_min 200); do
    x=0
    y=0
    current_x_vel=$x_vel
    current_y_vel=$y_vel
    highest_y=0

    while (( x <= x_max && y >= y_min )); do
      (( x += current_x_vel ))
      (( y += current_y_vel ))
      (( highest_y = highest_y > y ? highest_y : y ))

      if (( x >= x_min && x <= x_max && y >= y_min && y <= y_max )); then
        if (( highest_y > max_height )); then
          max_height=$highest_y
        fi
        valid_velocities+=("$x_vel,$y_vel")
        break
      fi

      (( current_x_vel > 0 )) && (( current_x_vel-- ))
      (( current_y_vel-- ))
    done
  done
done

# Output results
echo "Highest Y Position: $max_height"
echo "Distinct Initial Velocities: $(printf "%s\n" "${valid_velocities[@]}" | sort -u | wc -l)"