
#!/bin/bash

set -euo pipefail

# Function to calculate Greatest Common Divisor (GCD) using Euclidean algorithm
gcd() {
  local a=$1 b=$2 temp
  while (( b != 0 )); do
    temp=$b
    b=$((a % b))
    a=$temp
  done
  echo "$a"
}

# Function to calculate Least Common Multiple (LCM)
lcm() {
  local a=$1 b=$2
  if (( a == 0 || b == 0 )); then
    echo 0
    return
  fi
  local common_divisor
  common_divisor=$(gcd "$a" "$b")
  # Calculate LCM as (a / gcd) * b to avoid potential overflow
  echo $(((a / common_divisor) * b))
}

main() {
  local -a totals starts
  local i=0
  local line total start

  # Read input file and parse disc information
  while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ "$line" =~ Disc\ #([0-9]+)\ has\ ([0-9]+)\ positions\;\ at\ time=0,\ it\ is\ at\ position\ ([0-9]+)\. ]]; then
      totals[i]="${BASH_REMATCH[2]}"
      starts[i]="${BASH_REMATCH[3]}"
      ((i++))
    else
      # Optionally handle lines that don't match
      # echo "Warning: Skipping malformed line: $line" >&2
      : # Do nothing for non-matching lines
    fi
  done < "input.txt"

  # Add the extra disc for Part 2
  totals[i]=11
  starts[i]=0
  ((i++))

  local num_discs=${#totals[@]}
  local time=0
  local step=1 # Represents the LCM of the moduli processed so far

  # Solve using the Chinese Remainder Theorem approach iteratively
  for ((j=0; j<num_discs; j++)); do
    total=${totals[j]}
    start=${starts[j]}
    # Find the first time 't' (starting from current 'time', incrementing by 'step')
    # such that (start + t + j + 1) % total == 0
    while ((( (start + time + j + 1) % total ) != 0 )); do
      time=$((time + step))
    done
    # Update the step size to the LCM of the current step and the current disc's total positions
    step=$(lcm "$step" "$total")
  done

  echo "$time"
}

# Call the main function
main
