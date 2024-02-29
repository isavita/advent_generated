#!/bin/bash

# Initialize variables
mask=""
declare -A mem

# Function to apply mask to a given value
apply_mask() {
  local value=$1
  local mask=$2
  local result=0

  for ((i=0; i<36; i++)); do
    local bitValue=$((1 << (35 - i)))
    local maskChar="${mask:$i:1}"
    if [[ $maskChar == "1" ]]; then
      ((result |= bitValue))
    elif [[ $maskChar == "X" ]]; then
      ((result |= (value & bitValue)))
    fi
  done

  echo $result
}

# Read and process the input file
while IFS= read -r line || [[ -n "$line" ]]; do
  if [[ $line == mask\ =\ * ]]; then
    mask=${line#"mask = "}
  elif [[ $line =~ mem\[([0-9]+)\]\ \=\ ([0-9]+) ]]; then
    address=${BASH_REMATCH[1]}
    value=${BASH_REMATCH[2]}
    maskedValue=$(apply_mask $value $mask)
    mem[$address]=$maskedValue
  fi
done < "input.txt"

# Calculate and print the sum of values in memory
sum=0
for value in "${mem[@]}"; do
  ((sum+=value))
done

echo $sum