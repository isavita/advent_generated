#!/usr/bin/awk -f
BEGIN {
  for (i = 0; i < 6; i++) for (j = 0; j < 50; j++) screen[i "," j] = 0
  while ((getline line < "input.txt") > 0) proc(line)
}
function proc(line,   parts, tmp, A, B, i, j, k, row, shift, col) {
  split(line, parts, /[ ]+/)
  if (parts[1] == "rect") {
    split(parts[2], tmp, "x"); A = tmp[1] + 0; B = tmp[2] + 0
    for (i = 0; i < B; i++) for (j = 0; j < A; j++) screen[i "," j] = 1
  } else if (parts[1] == "rotate" && parts[2] == "row") {
    split(parts[3], tmp, "="); row = tmp[2] + 0
    shift = parts[5] + 0
    for (k = 0; k < 50; k++) newRow[k] = 0
    for (i = 0; i < 50; i++) newRow[(i + shift) % 50] = screen[row "," i]
    for (i = 0; i < 50; i++) screen[row "," i] = newRow[i]
  } else if (parts[1] == "rotate" && parts[2] == "column") {
    split(parts[3], tmp, "="); col = tmp[2] + 0
    shift = parts[5] + 0
    for (k = 0; k < 6; k++) newCol[k] = 0
    for (i = 0; i < 6; i++) newCol[(i + shift) % 6] = screen[i "," col]
    for (i = 0; i < 6; i++) screen[i "," col] = newCol[i]
  }
}
END {
  lit = 0
  for (i = 0; i < 6; i++) for (j = 0; j < 50; j++) if (screen[i "," j]) lit++
  print lit
}