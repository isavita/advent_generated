#!/usr/bin/env bash
# Advent of Code - Day 8 Part 1: Haunted Wasteland
# Reads from input.txt and prints the number of steps to reach ZZZ from AAA.

set -euo pipefail

main() {
  local file="input.txt"
  [[ -f "$file" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
    BEGIN {
      instr = ""; n = 0; start = "AAA"; target = "ZZZ";
    }
    NR == 1 {
      gsub(/[ \t\r]/,"");
      instr = $0;
      n = length(instr);
      next;
    }
    NF == 0 { next }  # skip blank lines
    {
      # Line format: NAME = (LEFT, RIGHT)
      # Extract tokens robustly
      gsub(/[=(),]/, " ");
      # Now fields: NAME LEFT RIGHT
      name = $1; left = $2; right = $3;
      L[name] = left;
      R[name] = right;
    }
    END {
      if (instr == "" || n == 0) { print 0; exit }
      cur = start;
      steps = 0;
      i = 1; # 1-based index into instr
      # Iterate until reach target
      while (cur != target) {
        c = substr(instr, i, 1);
        if (c == "L") {
          cur = L[cur];
        } else if (c == "R") {
          cur = R[cur];
        } else {
          # Ignore any non L/R characters just in case
        }
        steps++;
        i++; if (i > n) i = 1;
      }
      print steps;
    }
  ' "$file"
}

main "$@"