#!/usr/bin/env bash
# Solve Advent of Code 2024 Day 4 (Ceres Search) Parts 1 and 2.
# Reads grid from input.txt and prints two numbers: part1_count part2_count

main() {
  local input="input.txt"
  [[ -f "$input" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
    BEGIN {
      target = "XMAS"
      tlen = length(target)
      # 8 directions: N, NE, E, SE, S, SW, W, NW
      dx[1]=-1; dy[1]= 0
      dx[2]=-1; dy[2]= 1
      dx[3]= 0; dy[3]= 1
      dx[4]= 1; dy[4]= 1
      dx[5]= 1; dy[5]= 0
      dx[6]= 1; dy[6]=-1
      dx[7]= 0; dy[7]=-1
      dx[8]=-1; dy[8]=-1
    }
    {
      ncols = length($0)
      for (j=1; j<=ncols; j++) grid[NR, j] = substr($0, j, 1)
      nrows = NR
    }
    END {
      # Part 1: count occurrences of XMAS in all 8 directions
      p1 = 0
      for (i=1; i<=nrows; i++) {
        for (j=1; j<=ncols; j++) {
          if (grid[i,j] != substr(target,1,1)) continue
          for (d=1; d<=8; d++) {
            ok = 1
            for (k=0; k<tlen; k++) {
              r = i + dx[d]*k
              c = j + dy[d]*k
              if (r<1 || r>nrows || c<1 || c>ncols) { ok=0; break }
              if (grid[r,c] != substr(target, k+1, 1)) { ok=0; break }
            }
            if (ok) p1++
          }
        }
      }

      # Part 2: count X-MAS patterns (two MAS on diagonals forming an X).
      # For each cell as center A, check diagonals: (NW<->SE) and (NE<->SW)
      # Each diagonal must form M-A-S or S-A-M with A at center.
      p2 = 0
      for (i=2; i<=nrows-1; i++) {
        for (j=2; j<=ncols-1; j++) {
          if (grid[i,j] != "A") continue

          nw = grid[i-1,j-1]; se = grid[i+1,j+1]
          ne = grid[i-1,j+1]; sw = grid[i+1,j-1]

          diag1_ok = ((nw=="M" && se=="S") || (nw=="S" && se=="M"))
          diag2_ok = ((ne=="M" && sw=="S") || (ne=="S" && sw=="M"))

          if (diag1_ok && diag2_ok) p2++
        }
      }

      print p1, p2
    }
  ' "$input"
}

main "$@"