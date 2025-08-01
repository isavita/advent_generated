#!/usr/bin/env bash
set -euo pipefail

read_input() {
  local t d
  t=$(sed -n '1s/.*://p' input.txt | tr -cd '0-9')
  d=$(sed -n '2s/.*://p' input.txt | tr -cd '0-9')
  echo "$t $d"
}

calc() {
  local T=$1 R=$2
  local l=0 h=$T m dist

  while [ $l -le $h ]; do
    m=$(( (l + h) / 2 ))
    dist=$(( m * (T - m) ))
    if [ $dist -gt $R ]; then
      h=$((m - 1))
    else
      l=$((m + 1))
    fi
  done
  local first=$l

  l=0; h=$T
  while [ $l -le $h ]; do
    m=$(( (l + h) / 2 ))
    dist=$(( m * (T - m) ))
    if [ $dist -gt $R ]; then
      l=$((m + 1))
    else
      h=$((m - 1))
    fi
  done
  local last=$h
  echo $(( last - first + 1 ))
}

main() {
  read T R < <(read_input)
  calc "$T" "$R"
}

main "$@"