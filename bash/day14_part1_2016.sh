#!/usr/bin/env bash
set -euo pipefail

salt=$(head -n1 input.txt | tr -d '\r\n')
keys=0
idx=0

md5hex() { printf '%s' "$1" | md5sum | awk '{print $1}'; }

find_triplet() {
  local h=$1 c i
  for ((i=0;i<${#h}-2;i++)); do
    c=${h:i:1}
    [[ ${h:i+1:1} == "$c" && ${h:i+2:1} == "$c" ]] && { printf '%s' "$c"; return; }
  done
  printf ''
}

declare -a cache

while (( keys < 64 )); do
  if [[ -z "${cache[$idx]-}" ]]; then
    cache[$idx]=$(md5hex "$salt$idx")
  fi
  h=${cache[$idx]}
  c=$(find_triplet "$h")
  if [[ -n "$c" ]]; then
    quint="${c}${c}${c}${c}${c}"
    found=0
    for ((i=1;i<=1000;i++)); do
      j=$((idx+i))
      if [[ -z "${cache[$j]-}" ]]; then
        cache[$j]=$(md5hex "$salt$j")
      fi
      [[ ${cache[$j]} == *"$quint"* ]] && { keys=$((keys+1)); found=1; break; }
    done
  fi
  idx=$((idx+1))
done

echo $((idx-1))