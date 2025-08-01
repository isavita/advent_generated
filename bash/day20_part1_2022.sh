#!/usr/bin/env bash
# AoC 2022 Day 20 Part 1 - Grove Positioning System
# Reads integers from input.txt, performs one mixing pass, prints sum of 1000th, 2000th, 3000th after zero.

set -euo pipefail

main() {
  local input="input.txt"
  [[ -f "$input" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
  function mod(a, m) { # mathematical modulo in [0, m-1]
    return ((a % m) + m) % m
  }
  # remove element at idx from pos array of length L, shifting left
  function array_remove(pos, idx, L,    i) {
    for (i = idx; i < L; i++) pos[i] = pos[i+1]
    delete pos[L]
  }
  # insert val into pos array at 1-based index idx, shifting right; L is current length BEFORE insert
  function array_insert(pos, idx, L, val,    i) {
    for (i = L; i >= idx; i--) pos[i+1] = pos[i]
    pos[idx] = val
  }
  BEGIN {
    n = 0
  }
  NF {
    val = $1 + 0
    n++
    vals[n] = val       # value by original id
    pos[n]  = n         # current sequence: sequence of original ids
  }
  END {
    if (n == 0) { print 0; exit }
    # Perform one mixing pass in original order 1..n
    for (id = 1; id <= n; id++) {
      v = vals[id]
      if (v == 0) continue

      # find current index of this id
      cur = -1
      for (i = 1; i <= n; i++) if (pos[i] == id) { cur = i; break }
      # remove it (list becomes length n-1)
      array_remove(pos, cur, n)
      L = n - 1

      # compute destination index in 1..(n-1)
      # moving by v in circular list of length L
      # base position is cur, but after removal positions contract; target insert is cur + v modulo L
      # zero-based: dest0 = (cur-1 + v) mod L; 1-based:
      dest = mod((cur - 1 + v), L) + 1

      array_insert(pos, dest, L, id)
    }

    # locate zero
    zero_idx = -1
    for (i = 1; i <= n; i++) {
      if (vals[pos[i]] == 0) { zero_idx = i; break }
    }
    if (zero_idx < 0) { print "0"; exit }

    off1 = ((zero_idx - 1 + 1000) % n) + 1
    off2 = ((zero_idx - 1 + 2000) % n) + 1
    off3 = ((zero_idx - 1 + 3000) % n) + 1

    a = vals[pos[off1]]
    b = vals[pos[off2]]
    c = vals[pos[off3]]

    print a + b + c
  }
  ' "$input"
}

main "$@"