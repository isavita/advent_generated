#!/usr/bin/env bash
set -euo pipefail

mapfile -t lines < input.txt
n=${#lines[@]}
arr=()
for s in "${lines[@]}"; do
  s="${s//$'\r'/}"
  [[ -n "$s" ]] && arr+=("$s")
done
n=${#arr[@]}
target=150

minCount=
ways=0

recurse() {
  local target_left=$1 idx=$2 cnt=$3
  if (( target_left == 0 )); then
    if [[ -z "${minCount-}" || $cnt -lt $minCount ]]; then
      minCount=$cnt
      ways=1
    elif (( cnt == minCount )); then
      ((ways++))
    fi
    return
  fi
  if (( target_left < 0 || idx >= n )); then
    return
  fi
  recurse $((target_left - arr[idx])) $((idx+1)) $((cnt+1))
  recurse $target_left $((idx+1)) $cnt
}

recurse "$target" 0 0
echo "$ways"