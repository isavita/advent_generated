#!/usr/bin/awk -f
BEGIN {
  infections = 0
  dx[0] = 0; dx[1] = 1; dx[2] = 0; dx[3] = -1
  dy[0] = -1; dy[1] = 0; dy[2] = 1; dy[3] = 0
  y = 0
  while ((getline line < "input.txt") > 0) {
    len = length(line)
    for (i = 1; i <= len; i++) {
      if (substr(line, i, 1) == "#") {
        row = i - 1
        key = row "" "," "" y
        infected[key] = 1
      }
    }
    width = len
    y++
  }
  height = y
  startX = (width - (width % 2)) / 2
  startY = (height - (height % 2)) / 2
  dir = 0
  x = startX
  yy = startY
  for (step = 0; step < 10000; step++) {
    key = x "" "," "" yy
    if (key in infected) {
      dir = (dir + 1) % 4
      delete infected[key]
    } else {
      dir = (dir - 1 + 4) % 4
      infected[key] = 1
      infections++
    }
    x += dx[dir]
    yy += dy[dir]
  }
  print infections
  exit
}