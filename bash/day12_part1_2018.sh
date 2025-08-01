#!/usr/bin/env bash
# Advent of Code 2018 - Day 12 Part 1
# Reads from input.txt and prints the sum of pot indices with plants after 20 generations.

set -euo pipefail

main() {
  local file="input.txt"
  [[ -f "$file" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
    BEGIN {
      gen_limit = 20
    }
    NR==1 {
      # Parse initial state
      sub(/^initial state: /, "", $0)
      init = $0
      next
    }
    NF==0 { next }  # skip blanks
    {
      # Parse rules: pattern => result
      pat = substr($0, 1, 5)
      res = substr($0, 10, 1)
      if (res == "#") grow[pat] = 1
      next
    }
    END {
      # Represent state as associative array of occupied pot indices
      split("", state)
      for (i = 1; i <= length(init); i++) {
        if (substr(init, i, 1) == "#") state[i-1] = 1
      }

      for (g = 1; g <= gen_limit; g++) {
        # Determine bounds
        min = 1e9; max = -1e9
        for (k in state) {
          ki = k + 0
          if (ki < min) min = ki
          if (ki > max) max = ki
        }
        # Grow with a margin of 2 each side due to 5-wide window
        split("", nextstate)
        for (p = min - 2; p <= max + 2; p++) {
          # Build 5-char window around p
          w = ""
          for (d = -2; d <= 2; d++) {
            w = w (( (p+d) in state ) ? "#" : ".")
          }
          if (w in grow) nextstate[p] = 1
        }
        delete state
        # Trim leading/trailing empty by rebuilding bounds implicitly next gen
        for (k in nextstate) state[k] = 1
      }

      # Sum indices with plants
      sum = 0
      for (k in state) sum += (k + 0)
      print sum
    }
  ' "$file"
}

main "$@"