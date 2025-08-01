#!/usr/bin/env bash
# AoC-like Day 15 Part 1: Warehouse Woes
# Reads input from input.txt and prints the sum of GPS coordinates after simulating moves.

set -euo pipefail

main() {
  local file="input.txt"
  [[ -f "$file" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
    BEGIN {
      FS = ""
      in_moves = 0
    }
    # Parse: grid first, then a blank line, then moves (possibly multiple lines)
    {
      if (!in_moves) {
        if ($0 ~ /^[[:space:]]*$/) { in_moves = 1; next }
        grid[++h] = $0
      } else {
        gsub(/[[:space:]]/, "", $0)
        moves = moves $0
      }
    }
    END {
      if (h == 0) { print 0; exit }
      # Convert grid to 2D array and find robot
      w = length(grid[1])
      for (i = 1; i <= h; i++) {
        if (length(grid[i]) != w) {
          # normalize or error; assume well-formed rectangle per problem
        }
        for (j = 1; j <= w; j++) {
          c = substr(grid[i], j, 1)
          G[i, j] = c
          if (c == "@") { ry = i; rx = j }
        }
      }

      # Direction map
      dir["^"] = "-1 0"
      dir["v"] = "1 0"
      dir["<"] = "0 -1"
      dir[">"] = "0 1"

      # Simulate moves
      for (k = 1; k <= length(moves); k++) {
        m = substr(moves, k, 1)
        if (!(m in dir)) continue
        split(dir[m], d, " ")
        dy = d[1] + 0
        dx = d[2] + 0

        nry = ry + dy
        nrx = rx + dx
        c = G[nry, nrx]

        if (c == "#") {
          # blocked by wall
          continue
        } else if (c == ".") {
          # move robot into empty
          G[ry, rx] = "."
          G[nry, nrx] = "@"
          ry = nry; rx = nrx
        } else if (c == "O") {
          # attempt to push a run of boxes in direction
          py = nry; px = nrx
          # find first non-box tile along direction
          while (G[py, px] == "O") { py += dy; px += dx }
          if (G[py, px] == "#") {
            # cannot push into wall
            continue
          } else if (G[py, px] == ".") {
            # shift boxes: from tail to head
            ty = py; tx = px
            while (!(ty == nry && tx == nrx)) {
              py2 = ty - dy; px2 = tx - dx
              G[ty, tx] = G[py2, px2]  # move box forward
              ty = py2; tx = px2
            }
            # place robot
            G[ry, rx] = "."
            G[nry, nrx] = "@"
            ry = nry; rx = nrx
          } else if (G[py, px] == "@") {
            # should not occur in front of robot
            continue
          }
        }
      }

      # Sum GPS coordinates of all boxes: 100*y + x (1-based indices)
      sum = 0
      for (i = 1; i <= h; i++) {
        for (j = 1; j <= w; j++) {
          if (G[i, j] == "O") sum += 100 * (i - 1) + (j - 1)
        }
      }
      print sum
    }
  ' "$file"
}

main "$@"