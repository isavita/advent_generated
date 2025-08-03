#!/usr/bin/env bash
set -euo pipefail

declare -a seen
max=0

while IFS= read -r line || [[ -n "$line" ]]; do
  [[ -z "$line" ]] && continue
  row=${line:0:7}
  col=${line:7:3}
  rowbin=${row//F/0}; rowbin=${rowbin//B/1}
  colbin=${col//L/0}; colbin=${colbin//R/1}
  seat=$((2#$rowbin*8 + 2#$colbin))
  seen[$seat]=1
  (( seat > max )) && max=$seat
done < input.txt

for ((i=8;i<1016;i++)); do
  if [[ -z "${seen[$i]:-}" && "${seen[$((i-1))]:-}" == 1 && "${seen[$((i+1))]:-}" == 1 ]]; then
    echo "$i"
    exit 0
  fi
done