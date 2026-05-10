
import "io" for File

var gridSize = 100
var steps = 100

// --------------------------------------------------------------------
// initialise grid with false
var grid = List.filled(gridSize, null)
for (i in 0...gridSize) {
  grid[i] = List.filled(gridSize, false)
}

// read input file (each line must contain exactly gridSize characters)
var text = File.read("input.txt")
var y = 0
for (line in text.split("\n")) {
  if (line.count == 0) continue
  for (x in 0...gridSize) {
    grid[x][y] = line[x] == "#"
  }
  y = y + 1
}

// corners are always on
grid[0][0] = true
grid[0][gridSize - 1] = true
grid[gridSize - 1][0] = true
grid[gridSize - 1][gridSize - 1] = true

// helper: count live neighbours of cell (x, y)
var countNeighbours = Fn.new { |g, x, y|
  var on = 0
  for (dx in -1..1) {
    for (dy in -1..1) {
      if (dx == 0 && dy == 0) continue
      var nx = x + dx
      var ny = y + dy
      if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && g[nx][ny]) {
        on = on + 1
      }
    }
  }
  return on
}

// run the simulation
for (s in 0...steps) {
  var newGrid = List.filled(gridSize, null)
  for (i in 0...gridSize) {
    newGrid[i] = List.filled(gridSize, false)
  }

  for (x in 0...gridSize) {
    for (y in 0...gridSize) {
      var n = countNeighbours.call(grid, x, y)
      if (grid[x][y]) {
        newGrid[x][y] = (n == 2 || n == 3)
      } else {
        newGrid[x][y] = (n == 3)
      }
    }
  }

  // keep the four corners lit
  newGrid[0][0] = true
  newGrid[0][gridSize - 1] = true
  newGrid[gridSize - 1][0] = true
  newGrid[gridSize - 1][gridSize - 1] = true

  grid = newGrid
}

// count lights that are on
var onCount = 0
for (row in grid) {
  for (light in row) {
    if (light) onCount = onCount + 1
  }
}

System.print(onCount)
