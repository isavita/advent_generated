
#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN {
  file = "input.txt"
  y = 0
  while ((getline line < file) > 0) {
    y++
    rows = y
    cols = length(line)
    for (x = 1; x <= cols; x++) grid[y, x] = substr(line, x, 1) + 0
  }
  close(file)
}

function low(x, y, h) {
  h = grid[y, x]
  if (x > 1 && grid[y, x - 1] <= h) return 0
  if (x < cols && grid[y, x + 1] <= h) return 0
  if (y > 1 && grid[y - 1, x] <= h) return 0
  if (y < rows && grid[y + 1, x] <= h) return 0
  return 1
}

function basin(x, y,   top, sx, sy, cx, cy, n, s) {
  if (visited[y, x] || grid[y, x] == 9) return 0
  top = 0
  sx[top] = x
  sy[top] = y
  top++
  n = 0
  while (top > 0) {
    top--
    cx = sx[top]
    cy = sy[top]
    if (visited[cy, cx] || grid[cy, cx] == 9) continue
    visited[cy, cx] = 1
    n++
    if (cx > 1) { sx[top] = cx - 1; sy[top] = cy; top++ }
    if (cx < cols) { sx[top] = cx + 1; sy[top] = cy; top++ }
    if (cy > 1) { sx[top] = cx; sy[top] = cy - 1; top++ }
    if (cy < rows) { sx[top] = cx; sy[top] = cy + 1; top++ }
  }
  return n
}

END {
  k = 0
  for (y = 1; y <= rows; y++)
    for (x = 1; x <= cols; x++)
      if (low(x, y)) sizes[++k] = basin(x, y)

  for (i = 1; i <= k; i++)
    for (j = i + 1; j <= k; j++)
      if (sizes[j] > sizes[i]) { t = sizes[i]; sizes[i] = sizes[j]; sizes[j] = t }

  ans = 1
  for (i = 1; i <= 3 && i <= k; i++) ans *= sizes[i]
  print ans
}
' input.txt
