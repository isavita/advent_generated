#!/bin/bash

input="input.txt"
totalRibbon=0

while IFS= read -r line || [[ -n "$line" ]]; do
  IFS='x' read -r -a dimensions <<< "$line"
  
  if [ "${#dimensions[@]}" -ne 3 ]; then
    echo "Invalid input format"
    exit 1
  fi
  
  l=${dimensions[0]}
  w=${dimensions[1]}
  h=${dimensions[2]}
  
  # Calculate ribbon for the bow
  bow=$((l * w * h))
  
  # Calculate ribbon for wrapping (smallest perimeter)
  # Sort dimensions to find the two smallest
  sorted_dimensions=($(echo "${dimensions[@]}" | tr ' ' '\n' | sort -n))
  wrap=$((2 * sorted_dimensions[0] + 2 * sorted_dimensions[1]))
  
  totalRibbon=$((totalRibbon + bow + wrap))
done < "$input"

echo "$totalRibbon"