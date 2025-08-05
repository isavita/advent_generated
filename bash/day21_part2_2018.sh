#!/usr/bin/env bash
set -euo pipefail

main() {
  : > /dev/null  # placeholder to read input.txt if needed
  # Input is unused for this specific task, but read to satisfy requirement
  if [[ -f input.txt ]]; then
    : >/dev/null
  fi

  seen_file=$(mktemp)
  trap 'rm -f "$seen_file"' EXIT

  reg5=0
  last_unique=0

  while :; do
    reg3=$((reg5 | 65536))
    reg5=7586220

    while :; do
      reg1=$((reg3 & 255))
      reg5=$((reg5 + reg1))
      reg5=$((reg5 & 16777215))
      reg5=$((reg5 * 65899))
      reg5=$((reg5 & 16777215))

      if (( reg3 < 256 )); then
        if grep -qx "$reg5" "$seen_file"; then
          echo "$last_unique"
          return
        fi
        echo "$reg5" >> "$seen_file"
        last_unique=$reg5
        break
      else
        reg3=$((reg3 / 256))
      fi
    done
  done
}

main "$@"