#!/usr/bin/awk -f
BEGIN {
  gridSize = 100
  steps = 100

  linesRead = 0
  while ((getline line < "input.txt") > 0 && linesRead < gridSize) {
    for (x = 0; x < gridSize && x < length(line); x++) {
      ch = substr(line, x + 1, 1)
      grid[linesRead, x] = (ch == "#") ? 1 : 0
    }
    linesRead++
  }
  close("input.txt")

  grid[0, 0] = 1
  grid[0, gridSize - 1] = 1
  grid[gridSize - 1, 0] = 1
  grid[gridSize - 1, gridSize - 1] = 1

  for (i = 0; i < steps; i++) step()

  total = 0
  for (y = 0; y < gridSize; y++) {
    for (x = 0; x < gridSize; x++) {
      if (grid[y, x]) total++
    }
  }
  print total
  exit
}

function countOnNeighbors(x, y,   dx, dy, nx, ny, on) {
  on = 0
  for (dx = -1; dx <= 1; dx++) {
    for (dy = -1; dy <= 1; dy++) {
      if (dx == 0 && dy == 0) continue
      nx = x + dx
      ny = y + dy
      if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize) {
        if (grid[ny, nx]) on++
      }
    }
  }
  return on
}

function step() {
  for (y = 0; y < gridSize; y++) {
    for (x = 0; x < gridSize; x++) {
      onNeighbors = countOnNeighbors(x, y)
      if (grid[y, x]) {
        newGrid[y, x] = (onNeighbors == 2 || onNeighbors == 3) ? 1 : 0
      } else {
        newGrid[y, x] = (onNeighbors == 3) ? 1 : 0
      }
    }
  }
  newGrid[0, 0] = 1
  newGrid[0, gridSize - 1] = 1
  newGrid[gridSize - 1, 0] = 1
  newGrid[gridSize - 1, gridSize - 1] = 1
  for (y = 0; y < gridSize; y++) {
    for (x = 0; x < gridSize; x++) {
      grid[y, x] = newGrid[y, x]
    }
  }
}