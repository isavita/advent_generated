#!/bin/bash

# Read directions from input.txt
input=$(<input.txt)

# Initialize positions for Santa and Robo-Santa
santa_x=0
santa_y=0
robo_x=0
robo_y=0

# Use a temporary file to track visited houses
tempfile=$(mktemp)

# Mark the starting house as visited by both Santa and Robo-Santa
echo "0,0" >> "$tempfile"
echo "0,0" >> "$tempfile"

# Function to update the visited houses
update_visited() {
  local x=$1
  local y=$2
  echo "$x,$y" >> "$tempfile"
}

# Iterate through the directions
for (( i=0; i<${#input}; i++ )); do
  # Select who is moving this turn
  if (( $i % 2 == 0 )); then
    # Santa's turn
    case ${input:$i:1} in
      '>') ((santa_x++));;
      '<') ((santa_x--));;
      '^') ((santa_y++));;
      'v') ((santa_y--));;
    esac
    update_visited $santa_x $santa_y
  else
    # Robo-Santa's turn
    case ${input:$i:1} in
      '>') ((robo_x++));;
      '<') ((robo_x--));;
      '^') ((robo_y++));;
      'v') ((robo_y--));;
    esac
    update_visited $robo_x $robo_y
  fi
done

# Sort and count unique houses
unique_houses=$(sort -u "$tempfile" | wc -l)
echo "Houses that receive at least one present: $unique_houses"

# Clean up
rm "$tempfile"
