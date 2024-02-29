#!/bin/bash

# Function to calculate absolute value
abs() {
  if [ $1 -lt 0 ]; then
    echo $((-$1))
  else
    echo $1
  fi
}

# Function to find maximum of two numbers
max() {
  if [ $1 -gt $2 ]; then
    echo $1
  else
    echo $2
  fi
}

# Function to calculate distance
distance() {
  local x=$(abs $1)
  local y=$(abs $2)
  local z=$(abs $3)
  echo $(((x + y + z) / 2))
}

# Read input from file
input=$(<input.txt)
IFS=',' read -ra directions <<< "$input"

x=0
y=0
z=0
maxDistance=0

# Iterate through each direction
for dir in "${directions[@]}"; do
  case $dir in
    n)
      ((y++))
      ((z--))
      ;;
    ne)
      ((x++))
      ((z--))
      ;;
    se)
      ((x++))
      ((y--))
      ;;
    s)
      ((y--))
      ((z++))
      ;;
    sw)
      ((x--))
      ((z++))
      ;;
    nw)
      ((x--))
      ((y++))
      ;;
  esac

  # Calculate current distance and update maxDistance
  curDistance=$(distance $x $y $z)
  maxDistance=$(max $maxDistance $curDistance)
done

echo $maxDistance