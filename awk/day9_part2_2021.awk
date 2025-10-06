#!/usr/bin/awk -f
BEGIN {
  file = "input.txt"
  lineNo = 0
  while ((getline line < file) > 0) {
    lineNo++
    if (lineNo == 1) cols = length(line)
    for (i = 1; i <= cols; i++) {
      height[lineNo, i] = substr(line, i, 1) + 0
    }
  }
  rows = lineNo
}
function isLowPoint(x, y,   h) {
  h = height[y, x]
  if (x > 1 && height[y, x-1] <= h) return 0
  if (x < cols && height[y, x+1] <= h) return 0
  if (y > 1 && height[y-1, x] <= h) return 0
  if (y < rows && height[y+1, x] <= h) return 0
  return 1
}
function exploreBasin(x, y,   sp, cx, cy, size) {
  if (visited[y, x] == 1) return 0
  if (height[y, x] == 9) return 0
  sp = 0
  stackX[sp] = x; stackY[sp] = y; sp++
  size = 0
  while (sp > 0) {
    sp--
    cx = stackX[sp]; cy = stackY[sp]
    if (visited[cy, cx] == 1) continue
    if (height[cy, cx] == 9) continue
    visited[cy, cx] = 1
    size++
    if (cx > 1) { stackX[sp] = cx-1; stackY[sp] = cy; sp++ }
    if (cx < cols) { stackX[sp] = cx+1; stackY[sp] = cy; sp++ }
    if (cy > 1) { stackX[sp] = cx; stackY[sp] = cy-1; sp++ }
    if (cy < rows) { stackX[sp] = cx; stackY[sp] = cy+1; sp++ }
  }
  return size
}
END {
  bCount = 0
  for (y = 1; y <= rows; y++) {
    for (x = 1; x <= cols; x++) {
      if (isLowPoint(x, y)) {
        bCount++
        sizes[bCount] = exploreBasin(x, y)
      }
    }
  }
  for (i = 1; i <= bCount; i++) {
    for (j = i+1; j <= bCount; j++) {
      if (sizes[i] < sizes[j]) {
        t = sizes[i]; sizes[i] = sizes[j]; sizes[j] = t
      }
    }
  }
  prod = 1
  limit = 3
  if (bCount < 3) limit = bCount
  for (i = 1; i <= limit; i++) prod *= sizes[i]
  print prod
}