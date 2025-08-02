#!/usr/bin/env bash
set -euo pipefail

awk -F' \\| ' '
function pts(win, your,   i,j,points) {
  split(win, W, /[[:space:]]+/)
  split(your, Y, /[[:space:]]+/)
  delete S
  for (i in W) if (W[i] ~ /^[0-9]+$/) S[W[i]]=1
  points=0
  for (j in Y)
    if (Y[j] ~ /^[0-9]+$/ && (Y[j] in S))
      points = points==0 ? 1 : points*2
  return points
}
{ if (NF==2) total += pts($1,$2) }
END { print total }
' input.txt