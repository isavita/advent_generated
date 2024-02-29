#!/bin/bash

preambleLength=25
numbers=()

while IFS= read -r line; do
  numbers+=("$line")
done < "input.txt"

for ((i=preambleLength; i<${#numbers[@]}; i++)); do
  number=${numbers[i]}
  is_valid=false

  for ((j=i-preambleLength; j<i; j++)); do
    prev_number=${numbers[j]}
    diff=$((number - prev_number))

    if [[ "${numbers[@]:j+1:i-j}" =~ $diff ]]; then
      is_valid=true
      break
    fi
  done

  if [ "$is_valid" = false ]; then
    echo $number
    break
  fi
done