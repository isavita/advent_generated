#!/bin/bash

input=$(cat input.txt)

decompress() {
  local input=$1 start=$2 end=$3
  local length=0 i=$start
  while [[ $i -lt $end ]]; do
    if [[ ${input:$i} =~ \(([0-9]+)x([0-9]+)\) ]]; then
      local charCount=${BASH_REMATCH[1]}
      local repeatCount=${BASH_REMATCH[2]}
      local markerLength=${#BASH_REMATCH}
      local nextIndex=$((i + markerLength))
      local decompressedInner=$(decompress "${input:$((nextIndex)):$charCount}" 0 $charCount)
      length=$((length + repeatCount * decompressedInner))
      i=$((nextIndex + charCount))
    else
      ((length++))
      ((i++))
    fi
  done
  echo $length
}

getDecompressedLengthV2() {
  local input=$1
  echo $(decompress "$input" 0 ${#input})
}

getDecompressedLengthV2 "$input"