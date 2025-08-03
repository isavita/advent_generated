#!/usr/bin/env bash
set -euo pipefail

main() {
  local s
  s=$(head -n1 input.txt | tr -d '\r\n')
  local n=${#s}
  local -a a b
  for ((i=0;i<n;i++)); do a[i]=${s:i:1}; done

  for ((phase=0; phase<100; phase++)); do
    for ((i=0;i<n;i++)); do
      local sum=0
      local run=$((i+1))
      local j=0
      while ((j<n)); do
        local start=$((j+run-1))
        local end=$((start+run-1))
        if ((start<n)); then
          (( start<0 )) && start=0
          (( end>=n )) && end=$((n-1))
          for ((k=start;k<=end;k++)); do sum=$((sum + a[k])); done
        fi
        j=$((j + 2*run))
        start=$((j+run-1))
        end=$((start+run-1))
        if ((start<n)); then
          (( start<0 )) && start=0
          (( end>=n )) && end=$((n-1))
          for ((k=start;k<=end;k++)); do sum=$((sum - a[k])); done
        fi
        j=$((j + 2*run))
      done
      b[i]=$(( sum<0 ? -sum%10 : sum%10 ))
    done
    a=("${b[@]}")
  done

  for ((i=0;i<8 && i<n;i++)); do printf %s "${a[i]}"; done
  printf '\n'
}

main "$@"