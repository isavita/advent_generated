#!/bin/bash

# Advent of Code 2018 Day 1 Part 2
# Usage: ./day1_part2_2018.sh [input_file]

input_file="${1:-input.txt}"

if [[ ! -r "$input_file" ]]; then
  echo "Error: cannot read '$input_file'" >&2
  exit 1
fi

# Read frequency changes into an array
mapfile -t changes < "$input_file"

freq=0
declare -A seen
seen[$freq]=1

while true; do
  for delta in "${changes[@]}"; do
    freq=$((freq + delta))
    if [[ -n ${seen[$freq]+1} ]]; then
      echo $freq
      exit 0
    fi
    seen[$freq]=1
  done
done

