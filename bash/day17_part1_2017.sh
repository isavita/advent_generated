#!/bin/bash

data=$(cat input.txt)
steps=$(echo $data | tr -d '\n')

buffer=(0)
currentPos=0

for (( i=1; i<=2017; i++ ))
do
  currentPos=$(( (currentPos + steps) % ${#buffer[@]} ))
  buffer=("${buffer[@]:0:$(($currentPos + 1))}" "$i" "${buffer[@]:$(($currentPos + 1))}")
  currentPos=$(( $currentPos + 1 ))
done

for (( i=0; i<${#buffer[@]}; i++ ))
do
  if [ ${buffer[i]} -eq 2017 ]
  then
    echo ${buffer[(($i + 1)) % ${#buffer[@]}]}
    break
  fi
done