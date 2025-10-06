#!/usr/bin/awk -f
BEGIN {
  MAX_LINES = 200
  MAX_WIDTH = 200
  MAX_GALAXIES = MAX_LINES * MAX_WIDTH
  width = 0
  height = 0
  n = 0
  EXPANSION_FACTOR = 1000000
  EXPANSION_ADD = EXPANSION_FACTOR - 1

  while ((getline line < "input.txt") > 0) {
    gsub(/\r$/, "", line)
    w = length(line)
    if (w == 0) continue
    if (width == 0) {
      width = w
      if (width > MAX_WIDTH) {
        print "Error: Line width " width " exceeds MAX_WIDTH " MAX_WIDTH > "/dev/stderr"
        exit 1
      }
    } else if (w != width) {
      printf("Error: Inconsistent line width (%d vs %d) at line %d\n", w, width, height+1) > "/dev/stderr"
      exit 1
    }
    for (x = 0; x < width; x++) {
      c = substr(line, x+1, 1)
      if (c == "#") {
        if (n >= MAX_GALAXIES) {
          print "Error: Too many galaxies" > "/dev/stderr"
          exit 1
        }
        gx[n] = x
        gy[n] = height
        row_has[height] = 1
        col_has[x] = 1
        n++
      }
    }
    height++
  }
  close("input.txt")

  current_offset = 0
  for (y = 0; y < height; y++) {
    row_offset[y] = current_offset
    if (!(y in row_has)) current_offset++
  }
  current_offset = 0
  for (x = 0; x < width; x++) {
    col_offset[x] = current_offset
    if (!(x in col_has)) current_offset++
  }

  total = 0
  if (n > 1) {
    for (i = 0; i < n; i++) {
      xi = gx[i]
      yi = gy[i]
      x1 = xi + col_offset[xi] * EXPANSION_ADD
      y1 = yi + row_offset[yi] * EXPANSION_ADD
      for (j = i + 1; j < n; j++) {
        x2 = gx[j] + col_offset[gx[j]] * EXPANSION_ADD
        y2 = gy[j] + row_offset[gy[j]] * EXPANSION_ADD
        dx = x1 - x2
        if (dx < 0) dx = -dx
        dy = y1 - y2
        if (dy < 0) dy = -dy
        total += dx + dy
      }
    }
  }

  print total
  exit
}