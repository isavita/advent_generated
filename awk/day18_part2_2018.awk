#!/usr/bin/awk -f
function transform() {
  for (i = 1; i <= N; i++) {
    row = ""
    for (j = 1; j <= M; j++) {
      c = substr(grid[i], j, 1)
      t = 0
      l = 0
      for (dx = -1; dx <= 1; dx++) {
        ni = i + dx
        if (ni < 1 || ni > N) continue
        for (dy = -1; dy <= 1; dy++) {
          nj = j + dy
          if (dx == 0 && dy == 0) continue
          if (nj < 1 || nj > M) continue
          ch = substr(grid[ni], nj, 1)
          if (ch == "|") t++
          else if (ch == "#") l++
        }
      }
      if (c == ".") {
        nc = (t >= 3) ? "|" : "."
      } else if (c == "|") {
        nc = (l >= 3) ? "#" : "|"
      } else if (c == "#") {
        nc = (l >= 1 && t >= 1) ? "#" : "."
      } else {
        nc = c
      }
      row = row nc
    }
    newGrid[i] = row
  }
  for (i = 1; i <= N; i++) grid[i] = newGrid[i]
}
BEGIN {
  N = 0
  while ((getline line < "input.txt") > 0) {
    lines[++N] = line
  }
  if (N == 0) {
    exit
  }
  for (i = 1; i <= N; i++) grid[i] = lines[i]
  M = length(lines[1])
  TOT = 1000000000
  minute = 0
  while (1) {
    sig = ""
    for (ii = 1; ii <= N; ii++) sig = sig grid[ii] "\n"
    if (sig in seen) {
      cycleStart = seen[sig]
      cycleLength = minute - cycleStart
      break
    }
    seen[sig] = minute
    transform()
    minute++
  }
  remaining = (TOT - cycleStart) % cycleLength
  for (r = 0; r < remaining; r++) transform()
  wooded = 0
  lumberyards = 0
  for (i = 1; i <= N; i++) {
    for (j = 1; j <= M; j++) {
      ch = substr(grid[i], j, 1)
      if (ch == "|") wooded++
      else if (ch == "#") lumberyards++
    }
  }
  printf("%d\n", wooded * lumberyards)
  exit
}