#!/usr/bin/env bash
# Advent of Code 2023 Day 11 Part 1 - Cosmic Expansion
# Reads from input.txt and prints the sum of shortest Manhattan distances after expansion (factor 2).

set -euo pipefail

main() {
  local input="input.txt"
  [[ -f "$input" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
    BEGIN {
      OFS=" ";
    }
    {
      line = $0
      grid[NR] = line
      ncols = length(line)
    }
    END {
      nrows = NR

      # Identify empty rows
      for (r = 1; r <= nrows; r++) {
        emptyRow[r] = 1
        for (c = 1; c <= ncols; c++) {
          ch = substr(grid[r], c, 1)
          if (ch == "#") { emptyRow[r] = 0; break }
        }
      }

      # Identify empty columns
      for (c = 1; c <= ncols; c++) {
        emptyCol[c] = 1
        for (r = 1; r <= nrows; r++) {
          ch = substr(grid[r], c, 1)
          if (ch == "#") { emptyCol[c] = 0; break }
        }
      }

      # Prefix sums of empty rows/cols to compute expanded coordinates.
      prefEmptyRow[0] = 0
      for (r = 1; r <= nrows; r++) prefEmptyRow[r] = prefEmptyRow[r-1] + emptyRow[r]

      prefEmptyCol[0] = 0
      for (c = 1; c <= ncols; c++) prefEmptyCol[c] = prefEmptyCol[c-1] + emptyCol[c]

      # Expansion factor for empty rows/cols is 2, i.e., add +1 per empty line passed.
      expFactor = 2

      # Collect galaxy coordinates and map to expanded coordinates.
      G = 0
      for (r = 1; r <= nrows; r++) {
        for (c = 1; c <= ncols; c++) {
          if (substr(grid[r], c, 1) == "#") {
            G++
            # Expanded coordinates: base + (factor-1) * count_of_empty_lines_before
            ex = r + (expFactor - 1) * prefEmptyRow[r]
            ey = c + (expFactor - 1) * prefEmptyCol[c]
            gx[G] = ex
            gy[G] = ey
          }
        }
      }

      # Sum pairwise Manhattan distances
      total = 0
      for (i = 1; i <= G; i++) {
        for (j = i + 1; j <= G; j++) {
          dx = gx[i] - gx[j]; if (dx < 0) dx = -dx
          dy = gy[i] - gy[j]; if (dy < 0) dy = -dy
          total += dx + dy
        }
      }

      print total
    }
  ' "$input"
}

main "$@"