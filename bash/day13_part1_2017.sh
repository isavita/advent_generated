#!/bin/bash

declare -A firewall
declare -A scanner_pos
declare -A scanner_dir

# Initialize the max depth
max_depth=0

# Read the input file and initialize the scanners
while IFS=": " read -r depth range; do
  firewall[$depth]=$range
  scanner_pos[$depth]=0
  scanner_dir[$depth]=1
  if [[ $depth -gt $max_depth ]]; then
    max_depth=$depth
  fi
done < "input.txt"

severity=0

# Simulate the packet moving through the firewall
for ((depth=0; depth<=max_depth; depth++)); do
  # Check if there's a scanner at this depth and its position
  if [[ -n ${firewall[$depth]} ]] && [[ ${scanner_pos[$depth]} -eq 0 ]]; then
    severity=$((severity + depth * firewall[$depth]))
  fi

  # Move all scanners
  for key in "${!scanner_pos[@]}"; do
    scanner_pos[$key]=$((scanner_pos[$key] + scanner_dir[$key]))

    # Change direction if needed
    if [[ ${scanner_pos[$key]} -eq 0 ]] || [[ ${scanner_pos[$key]} -eq $((firewall[$key] - 1)) ]]; then
      scanner_dir[$key]=$((scanner_dir[$key] * -1))
    fi
  done
done

echo $severity