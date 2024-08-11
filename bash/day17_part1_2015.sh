#!/bin/bash

# Read input from file
containers=($(cat input.txt))

# Initialize count of combinations
count=0

# Function to recursively try combinations
try_combinations() {
  local remaining=$1
  local index=$2
  if (( remaining == 0 )); then
    ((count++))
    return
  fi
  if (( index >= ${#containers[@]} )); then
    return
  fi
  if (( remaining - containers[index] >= 0 )); then
    try_combinations $((remaining - containers[index])) $((index + 1))
  fi
  try_combinations $remaining $((index + 1))
}

# Start trying combinations
try_combinations 150 0

# Print result
echo "$count"