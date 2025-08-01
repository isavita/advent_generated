#!/usr/bin/env bash
set -euo pipefail

main() {
  map=$(tr -d '\n' < input.txt)
  [ -z "$map" ] && echo 0 && exit 0

  # Pass 1: total size
  total=0
  for ((i=0;i<${#map};i++)); do
    d=${map:i:1}
    if [[ $d =~ [0-9] ]]; then
      (( total += d ))
    fi
  done
  [ "$total" -eq 0 ] && echo 0 && exit 0

  # Build disk as a bash array of ints; -1 denotes free
  declare -a disk
  pos=0
  idx=0
  for ((i=0;i<${#map};i++)); do
    d=${map:i:1}
    [[ $d =~ [0-9] ]] || continue
    len=$d
    if (( idx % 2 == 0 )); then
      fid=$(( idx / 2 ))
      for ((k=0;k<len;k++)); do
        disk[pos]=$fid
        ((pos++))
      done
    else
      for ((k=0;k<len;k++)); do
        disk[pos]=-1
        ((pos++))
      done
    fi
    ((idx++))
  done

  # Two-pointer compaction
  left=0
  right=$(( total - 1 ))
  while (( left < right )); do
    while (( left < right && disk[left] != -1 )); do ((left++)); done
    while (( left < right && disk[right] == -1 )); do ((right--)); done
    if (( left < right )); then
      disk[left]=${disk[right]}
      disk[right]=-1
      ((left++))
      ((right--))
    fi
  done

  # Checksum using awk for 64-bit arithmetic
  # Emit index and value, skip -1, compute sum(i*val)
  {
    for ((i=0;i<total;i++)); do
      printf "%d %d\n" "$i" "${disk[i]:--1}"
    done
  } | awk '$2!=-1{sum+=($1*$2)}END{printf "%s\n", sum+0}'
}

main "$@"