#!/usr/bin/env bash
# Day 3: Gear Ratios (Part 1) - Sum of part numbers adjacent to any symbol (non-dot)
# Reads from input.txt and prints the sum to stdout.

set -euo pipefail

main() {
  local input="input.txt"
  [[ -f "$input" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
    BEGIN {
      # Preload all lines into array L[0..n-1]
      n = 0
    }
    {
      L[n] = $0
      n++
    }
    END {
      sum = 0
      for (i = 0; i < n; i++) {
        line = L[i]
        len = length(line)

        # Scan for numbers in this line
        j = 1
        while (j <= len) {
          c = substr(line, j, 1)
          if (c ~ /[0-9]/) {
            # Start of a number token
            start = j
            numstr = c
            j++
            # consume remaining digits
            while (j <= len) {
              c = substr(line, j, 1)
              if (c ~ /[0-9]/) {
                numstr = numstr c
                j++
              } else {
                break
              }
            }
            end = j - 1

            # Check adjacency in 8 directions around the number span
            if (is_adjacent_symbol(L, n, i, start, end)) {
              sum += numstr + 0
            }
          } else {
            j++
          }
        }
      }
      print sum
    }

    # Helper: returns 1 if any neighbor cell around [i][start..end] contains a non-dot, non-digit symbol
    function is_adjacent_symbol(L, n, i, start, end,    r, cstart, cend, rmin, rmax, len, ch) {
      rmin = (i > 0) ? i - 1 : i
      rmax = (i < n - 1) ? i + 1 : i

      # Columns to check: from start-1 to end+1, clamped to line length
      len = length(L[i])
      cstart = start - 1; if (cstart < 1) cstart = 1
      cend   = end + 1;   if (cend > len) cend = len

      for (r = rmin; r <= rmax; r++) {
        lineLen = length(L[r])
        # Clamp cstart/cend to current row length
        cs = cstart; if (cs > lineLen) cs = lineLen
        ce = cend;   if (ce > lineLen) ce = lineLen
        if (cs < 1) cs = 1
        if (ce < 1) continue
        for (cc = cs; cc <= ce; cc++) {
          # Skip cells that overlap the number itself on the same row
          if (r == i && cc >= start && cc <= end) continue
          ch = substr(L[r], cc, 1)
          if (ch != "." && ch !~ /[0-9]/ && ch != "") return 1
        }
      }
      return 0
    }
  ' "$input"
}

main "$@"