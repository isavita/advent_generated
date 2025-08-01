#!/usr/bin/env bash
# Reads a grid of single-digit heights from input.txt and prints the count of trees
# visible from outside the grid (looking along rows/columns).

set -euo pipefail

main() {
  [[ -f input.txt ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
  BEGIN {
    FS=""
  }
  {
    # Read each row into grid with 0-based indices
    row = NR-1
    for (i=1; i<=NF; i++) {
      grid[row, i-1] = $i + 0
    }
    cols = NF
  }
  END {
    rows = NR
    if (rows == 0 || cols == 0) { print 0; exit }

    # Precompute max from each direction for O(1) visibility checks
    # Initialize arrays as associative with keys "r,c"
    # left_max[r,c]: max to the left of (exclusive)
    # right_max[r,c]: max to the right of (exclusive)
    # up_max[r,c]: max above (exclusive)
    # down_max[r,c]: max below (exclusive)

    # Left and right scans
    for (r=0; r<rows; r++) {
      m = -1
      for (c=0; c<cols; c++) {
        left_max[r,c] = m
        v = grid[r,c]
        if (v > m) m = v
      }
      m = -1
      for (c=cols-1; c>=0; c--) {
        right_max[r,c] = m
        v = grid[r,c]
        if (v > m) m = v
      }
    }

    # Up and down scans
    for (c=0; c<cols; c++) {
      m = -1
      for (r=0; r<rows; r++) {
        up_max[r,c] = m
        v = grid[r,c]
        if (v > m) m = v
      }
      m = -1
      for (r=rows-1; r>=0; r--) {
        down_max[r,c] = m
        v = grid[r,c]
        if (v > m) m = v
      }
    }

    visible = 0
    for (r=0; r<rows; r++) {
      for (c=0; c<cols; c++) {
        # Edge trees are visible
        if (r==0 || c==0 || r==rows-1 || c==cols-1) {
          visible++
          continue
        }
        h = grid[r,c]
        # Visible if strictly taller than all trees in any one direction
        if (h > left_max[r,c] || h > right_max[r,c] || h > up_max[r,c] || h > down_max[r,c]) {
          visible++
        }
      }
    }
    print visible
  }' input.txt
}

main "$@"