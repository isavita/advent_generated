#!/usr/bin/env bash
set -euo pipefail

input=$(<input.txt)
max=1000000

# Use awk for efficient numeric loops and array handling
awk -v target="$input" -v max="$max" '
BEGIN {
  for (i = 1; i < max; i++) {
    for (j = i; j < max; j += i) houses[j] += i * 10
  }
  for (i = 1; i < max; i++) {
    if (houses[i] >= target) { print i; exit }
  }
  print 0
}' </dev/null