#!/usr/bin/env bash
# Advent of Code 2023 - Day 12: Hot Springs
# Reads input from input.txt and writes answers (part 1 and part 2) to stdout.

set -euo pipefail

main() {
  local infile="input.txt"
  [[ -f "$infile" ]] || { echo "input.txt not found" >&2; exit 1; }

  awk '
  function count_arrangements(pattern, n_groups,   i, j, c, m, groups, memo_key, res) {
      # Parse groups string into array g[1..k]
      split(n_groups, groups, ",")
      k = length(groups)
      # Memoization cache is global assoc array memo[]
      delete memo
      # Prepare string -> array mapping for faster membership checks not needed in awk

      return dfs(pattern, 1, groups, 1)
  }

  # DFS with memoization.
  # s: pattern string
  # i: current index in s (1-based)
  # g[]: array of groups targets
  # gi: current group index (1-based)
  function dfs(s, i, g, gi,    key, n, L, ch, need, j, ok, res) {
      n = length(s)
      key = i "|" gi
      if (key in memo) return memo[key]

      # Skip forced dots and optional dots
      while (i <= n) {
          ch = substr(s, i, 1)
          if (ch == ".") { i++ ; continue }
          if (ch == "?") {
              # We can set this ? to dot and try skipping; but we only skip dots here.
              # Stop here to branch later.
              break
          }
          # if it is a # we must try placing a group here; break to handle below
          if (ch == "#") break
      }

      if (i > n) {
          # End of string: valid if all groups consumed
          memo[key] = (gi > length(g)) ? 1 : 0
          return memo[key]
      }

      res = 0
      ch = substr(s, i, 1)

      # Option 1: place a group starting at i if possible (only if we still have a group left)
      if (gi <= length(g)) {
          need = g[gi] + 0
          # Check segment s[i..i+need-1] all # or ? and within bounds
          if (i + need - 1 <= n) {
              ok = 1
              for (j = 0; j < need; j++) {
                  L = substr(s, i + j, 1)
                  if (L == ".") { ok = 0; break }
              }
              # Next must be dot or end (separator), i+need is either out of range or .,?
              if (ok) {
                  if (i + need <= n) {
                      L = substr(s, i + need, 1)
                      if (L == "#") ok = 0
                  }
              }
              if (ok) {
                  # Build next index: skip the group and one separator if exists and not end
                  if (i + need <= n && substr(s, i + need, 1) != ".") {
                      # If it is "?" we force it to dot by moving i by 1
                      res += dfs(force_char(s, i + need, "."), i + need + 1, g, gi + 1)
                  } else {
                      res += dfs(s, i + need + 1, g, gi + 1)
                  }
              }
          }
      }

      # Option 2: if current char can be dot (either ? or . already handled above),
      # try skipping as dot (only if current is ?; plain . already skipped in the loop)
      if (ch == "?") {
          res += dfs(force_char(s, i, "."), i + 1, g, gi)
      }

      memo[key] = res
      return res
  }

  # Return string s with position pos replaced by character c
  function force_char(s, pos, c,    pre, post) {
      pre = (pos > 1) ? substr(s, 1, pos - 1) : ""
      post = (pos < length(s)) ? substr(s, pos + 1) : ""
      return pre c post
  }

  # Count arrangements for one line, part 1
  function solve_line(line,    a, patt, grp, cleaned) {
      if (line == "") return 0
      split(line, a, /[ ]+/)
      patt = a[1]
      grp  = a[2]
      # Normalize pattern: collapse multiple dots into single dot not necessary; keep as is
      return count_arrangements(patt, grp)
  }

  # Build unfolded pattern and groups for part 2
  function unfold(patt, grp,    i, up, ug, tmp) {
      up = patt
      ug = grp
      for (i = 2; i <= 5; i++) up = up "?" patt
      for (i = 2; i <= 5; i++) ug = ug "," grp
      return up "|" ug
  }

  BEGIN {
      part1 = 0
      part2 = 0
  }
  {
      # Each line: pattern groups
      if ($0 ~ /^[[:space:]]*$/) next
      split($0, fields, /[ ]+/)
      patt = fields[1]
      grp  = fields[2]

      # Part 1
      delete memo
      p1 = count_arrangements(patt, grp)
      part1 += p1

      # Part 2
      split(unfold(patt, grp), uv, /\|/)
      up = uv[1]; ug = uv[2]
      delete memo
      p2 = count_arrangements(up, ug)
      part2 += p2
  }
  END {
      print part1
      print part2
  }
  ' "$infile"
}

main "$@"