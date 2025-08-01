#!/usr/bin/env bash
# Advent of Code 2022 - Day 5: Supply Stacks (Part 1)
# Reads from input.txt and prints the message formed by the top crate of each stack after moves.

set -euo pipefail

main() {
  awk '
  BEGIN {
    parsing_stacks = 1
    maxw = 0
  }
  # Helper to trim spaces
  function rtrim(s){ sub(/[ \t\r\n]+$/, "", s); return s }
  function ltrim(s){ sub(/^[ \t\r\n]+/, "", s); return s }
  function trim(s){ return rtrim(ltrim(s)) }

  # Parse initial drawing lines until the blank line
  parsing_stacks {
    if ($0 ~ /^[[:space:]]*$/) { parsing_stacks=0; init_stacks(); next }
    lines[++ln] = $0
    if (length($0) > maxw) maxw = length($0)
    next
  }

  # After blank line: movement instructions
  !parsing_stacks {
    if ($0 ~ /^move[[:space:]]+[0-9]+[[:space:]]+from[[:space:]]+[0-9]+[[:space:]]+to[[:space:]]+[0-9]+$/) {
      # Extract X from Y to Z
      n = $0
      gsub(/move[[:space:]]+/, "", n)
      split(n, a, /[[:space:]]+from[[:space:]]+/)
      cnt = a[1]
      split(a[2], b, /[[:space:]]+to[[:space:]]+/)
      from = b[1]; to = b[2]
      for (i=0;i<cnt;i++){
        if (top[from] == 0) continue
        val = stacks[from, top[from]]
        top[from]--
        stacks[to, ++top[to]] = val
      }
    }
  }
  END {
    # Output the top crate label of each stack concatenated
    out = ""
    for (i=1;i<=S;i++) {
      if (top[i] > 0) {
        v = stacks[i, top[i]]
        # v is like "[A]" -> extract A
        sub(/^\[/, "", v); sub(/\]$/, "", v)
        out = out v
      }
    }
    print out
  }

  # Initialize stacks by parsing stored lines
  function init_stacks(   i,j,row,last,idx,ch,col,label,pos) {
    # The last line has the stack numbers; count them
    last = lines[ln]
    # Count numbers by scanning digits groups
    S = 0
    for (i=1; i<=length(last); i++) {
      c = substr(last,i,1)
      if (c ~ /[0-9]/) {
        # Start of a number, increment stack count and skip the rest of number
        S++
        while (i<=length(last) && substr(last,i,1) ~ /[0-9]/) i++
      }
    }
    # Initialize tops
    for (i=1;i<=S;i++) top[i]=0

    # Each stack column is at positions 1,5,9,... -> pos = 1 + 4*(k-1)
    # Process drawing lines above the numbers from bottom to top
    for (row = ln-1; row >= 1; row--) {
      for (j=1; j<=S; j++) {
        pos = 1 + 4*(j-1)
        if (pos+2 <= length(lines[row])) {
          label = substr(lines[row], pos, 3)
          if (label ~ /^\[[A-Z]\]$/) {
            stacks[j, ++top[j]] = label
          }
        }
      }
    }
  }
  ' input.txt
}

main "$@"