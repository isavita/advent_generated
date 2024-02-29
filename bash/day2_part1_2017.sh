#!/bin/bash

input_file="input.txt"
checksum=0

while IFS= read -r line || [[ -n "$line" ]]; do
  nums=($line)
  minVal=${nums[0]}
  maxVal=${nums[0]}

  for num in "${nums[@]}"; do
    if [[ $num -lt $minVal ]]; then
      minVal=$num
    fi
    if [[ $num -gt $maxVal ]]; then
      maxVal=$num
    fi
  done

  diff=$((maxVal - minVal))
  checksum=$((checksum + diff))
done < "$input_file"

echo $checksum