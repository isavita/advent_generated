#!/usr/bin/awk -f
BEGIN {
  height = 0
  width = 0
  while ((getline line < "input.txt") > 0) {
    height++
    grid[height] = line
    if (length(line) > width) width = length(line)
  }
  close("input.txt")
}
function moveEast() {
  localMoved = 0
  for (y = 1; y <= height; y++) {
    for (x = 1; x <= width; x++) {
      key = y "," x
      newChar[key] = substr(grid[y], x, 1)
    }
  }
  for (y = 1; y <= height; y++) {
    for (x = 1; x <= width; x++) {
      orig = substr(grid[y], x, 1)
      if (orig == ">") {
        nextX = (x == width ? 1 : x + 1)
        if (substr(grid[y], nextX, 1) == ".") {
          newChar[y "," x] = "."
          newChar[y "," nextX] = ">"
          localMoved = 1
        }
      }
    }
  }
  for (y = 1; y <= height; y++) {
    s = ""
    for (x = 1; x <= width; x++) {
      s = s newChar[y "," x]
    }
    grid[y] = s
  }
  return localMoved
}
function moveSouth() {
  moved = 0
  for (y = 1; y <= height; y++) {
    for (x = 1; x <= width; x++) {
      newChar[y "," x] = substr(grid[y], x, 1)
    }
  }
  for (y = 1; y <= height; y++) {
    for (x = 1; x <= width; x++) {
      orig = substr(grid[y], x, 1)
      if (orig == "v") {
        nextY = (y == height ? 1 : y + 1)
        if (substr(grid[nextY], x, 1) == ".") {
          newChar[y "," x] = "."
          newChar[nextY "," x] = "v"
          moved = 1
        }
      }
    }
  }
  for (y = 1; y <= height; y++) {
    s = ""
    for (x = 1; x <= width; x++) {
      s = s newChar[y "," x]
    }
    grid[y] = s
  }
  return moved
}
END {
  steps = 0
  while (1) {
    east = moveEast()
    south = moveSouth()
    steps++
    if (east == 0 && south == 0) break
  }
  print steps
}