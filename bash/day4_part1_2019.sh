#!/bin/bash

read -r rangeStr < input.txt
IFS='-' read -r start end <<< "$rangeStr"

count=0

for ((i=start; i<=end; i++)); do
  s="$i"
  hasDouble=false
  prevChar=${s:0:1}
  for ((j=1; j<${#s}; j++)); do
    currChar=${s:j:1}
    if [ "$prevChar" == "$currChar" ]; then
      hasDouble=true
    fi
    if [ "$prevChar" \> "$currChar" ]; then
      hasDouble=false
      break
    fi
    prevChar="$currChar"
  done
  if $hasDouble; then
    ((count++))
  fi
done

echo $count