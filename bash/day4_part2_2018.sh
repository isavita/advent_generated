#!/usr/bin/env bash
# Advent of Code 2018 Day 4 - Repose Record
# Reads input from input.txt and prints two answers (Part 1 and Part 2).
# Requires: bash, sort, awk

set -euo pipefail

main() {
  input="input.txt"

  # Sort logs chronologically, then parse and compute:
  # - Total sleep per guard
  # - Per-minute sleep histogram per guard (0..59)
  # Finally, compute:
  # Part 1: guard with max total sleep; pick their most-slept minute; print guard_id * minute
  # Part 2: guard-minute pair with highest frequency across all guards; print guard_id * minute
  sort -o /tmp/repose_sorted.$$ "$input"

  awk -F'[][]' '
    function fin_entry(g, mstart, mend,   m) {
      # Accumulate per-minute histogram and totals for guard g
      for (m = mstart; m < mend; m++) {
        key = g ":" m
        gc[key]++
        gtot[g]++
      }
    }
    BEGIN {
      OFS=" "
    }
    {
      # Split fields: $2 has "YYYY-MM-DD hh:mm", $3 has the message
      ts = $2
      msg = $3

      # Extract minute
      split(ts, T, " ")
      split(T[2], HM, ":")
      minute = HM[2] + 0

      if (msg ~ /Guard #[0-9]+ begins shift/) {
        # New guard shift
        match(msg, /#[0-9]+/)
        guard = substr(msg, RSTART+1, RLENGTH-1) + 0
        asleep = -1
      } else if (msg ~ /falls asleep/) {
        asleep = minute
      } else if (msg ~ /wakes up/) {
        if (asleep >= 0) fin_entry(guard, asleep, minute)
        asleep = -1
      }
    }
    END {
      # Part 1: guard with max total sleep
      max_total = -1
      for (g in gtot) {
        if (gtot[g] > max_total) {
          max_total = gtot[g]
          best_guard = g + 0
        }
      }
      # Most frequent minute for best_guard
      best_min = -1; best_cnt = -1
      for (m = 0; m < 60; m++) {
        k = best_guard ":" m
        c = (k in gc) ? gc[k] : 0
        if (c > best_cnt) { best_cnt = c; best_min = m }
      }
      part1 = best_guard * best_min
      print part1

      # Part 2: overall guard-minute with highest frequency
      g2 = -1; m2 = -1; c2 = -1
      for (k in gc) {
        c = gc[k]
        # split k = "guard:minute"
        split(k, S, ":")
        gg = S[1] + 0
        mm = S[2] + 0
        if (c > c2) { c2 = c; g2 = gg; m2 = mm }
      }
      part2 = g2 * m2
      print part2
    }
  ' "/tmp/repose_sorted.$$"

  rm -f "/tmp/repose_sorted.$$"
}

main "$@"