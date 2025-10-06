#!/usr/bin/awk -f
BEGIN {
  algorithm = ""
  rows = 0
  cols = 0

  if ((getline line < "input.txt") <= 0) exit
  algorithm = line

  while ((getline line < "input.txt") > 0) {
    if (length(line) <= 1) continue
    if (cols == 0) cols = length(line)
    else if (length(line) != cols) exit 0
    image[rows] = line
    rows++
  }

  times = 2
  for (t = 0; t < times; t++) {
    flip = ((t % 2) == 1) && (substr(algorithm, 1, 1) == "#")
    new_rows = rows + 2
    new_cols = cols + 2
    for (i = 0; i < new_rows; i++) {
      line_out = ""
      for (j = 0; j < new_cols; j++) {
        idx = calc_index(i - 1, j - 1, rows, cols, flip)
        line_out = line_out substr(algorithm, idx + 1, 1)
      }
      new_image[i] = line_out
    }
    for (r = 0; r < rows; r++) delete image[r]
    for (r = 0; r < new_rows; r++) image[r] = new_image[r]
    delete new_image
    rows = new_rows
    cols = new_cols
  }

  lit = 0
  for (r = 0; r < rows; r++) {
    for (k = 1; k <= length(image[r]); k++) {
      if (substr(image[r], k, 1) == "#") lit++
    }
  }
  print lit
  exit
}

function calc_index(i, j, r, c, flip,   di, dj, ni, nj, idx, ch) {
  idx = 0
  for (di = -1; di <= 1; di++) {
    for (dj = -1; dj <= 1; dj++) {
      idx = idx * 2
      ni = i + di
      nj = j + dj
      if (ni >= 0 && ni < r && nj >= 0 && nj < c) {
        ch = substr(image[ni], nj + 1, 1)
        if (ch == "#") idx = idx + 1
      } else if (flip) {
        idx = idx + 1
      }
    }
  }
  return idx
}