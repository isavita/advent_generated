#!/usr/bin/env bash
# Advent of Code 2021 Day 4 - Giant Squid
# Reads from input.txt and prints:
# Part 1: score of the first winning board
# Part 2: score of the last winning board

set -euo pipefail

main() {
  awk -v FS="" '
  function trim(s){ gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", s); return s }
  function split_csv(line, arr,    n,i,tok) {
    n = split(line, tok, /,/)
    delete arr
    for (i=1;i<=n;i++) arr[i] = tok[i]+0
    return n
  }
  function parse_board(start,    r,c,t,k) {
    # board index = B
    B++
    for (r=0;r<5;r++) {
      # read next non-empty line
      line = lines[start + r]
      gsub(/^ +| +$/, "", line)
      # split by spaces (multiple)
      k = split(line, t, / +/)
      for (c=0;c<5;c++) {
        boards[B, r, c] = t[c+1]+0
        mark[B, r, c] = 0
      }
      rowc[B, r] = 0
    }
    for (c=0;c<5;c++) colc[B, c] = 0
    won[B] = 0
    # map number -> list of positions (append as "B;r;c")
    for (r=0;r<5;r++)
      for (c=0;c<5;c++) {
        v = boards[B,r,c]
        key = "n:" v
        idx = npos_count[key] + 1
        npos_count[key] = idx
        npos[key, idx] = B ";" r ";" c
      }
  }
  function score_board(b, last,    r,c,sum) {
    sum = 0
    for (r=0;r<5;r++)
      for (c=0;c<5;c++)
        if (!mark[b,r,c]) sum += boards[b,r,c]
    return sum * last
  }
  BEGIN {
    # Read all lines first
    while ((getline L < "input.txt") > 0) lines[++LN] = L
    if (LN == 0) { print "No input"; exit 1 }

    # Parse draws
    n_draws = split(lines[1], tmp, /,/)
    for (i=1;i<=n_draws;i++) draws[i] = tmp[i]+0

    # Parse boards
    B = 0
    i = 2
    while (i <= LN) {
      # skip blank lines
      while (i <= LN && trim(lines[i]) == "") i++
      if (i > LN) break
      # read 5 lines for a board
      parse_board(i)
      i += 5
    }

    totalBoards = B
    firstScore = -1
    lastScore = -1
    remaining = totalBoards

    # Play
    for (d=1; d<=n_draws; d++) {
      num = draws[d]
      key = "n:" num
      count = npos_count[key]
      for (k=1; k<=count; k++) {
        split(npos[key, k], pc, /;/)
        b = pc[1]+0; r = pc[2]+0; c = pc[3]+0
        if (won[b] || mark[b,r,c]) continue
        mark[b,r,c] = 1
        if (++rowc[b,r] == 5 || ++colc[b,c] == 5) {
          if (!won[b]) {
            won[b] = 1
            sc = score_board(b, num)
            if (firstScore < 0) firstScore = sc
            lastScore = sc
            remaining--
          }
        }
      }
      if (remaining == 0) break
    }

    print firstScore
    print lastScore
  }'
}

main "$@"