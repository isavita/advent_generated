import "io" for File

var input = File.read("input.txt")
var lines = input.split("\n")

var grid = []
for (line in lines) {
  var row = []
  for (char in line) {
    if (char == "#") {
      row.add(true)
    } else {
      row.add(false)
    }
  }
  grid.add(row)
}

var steps = 100
for (i in 1..steps) {
  var newGrid = []
  for (y in 0..grid.count - 1) {
    var row = []
    for (x in 0..grid[y].count - 1) {
      var onNeighbors = 0
      for (dy in -1..1) {
        for (dx in -1..1) {
          var ny = y + dy
          var nx = x + dx
          if (ny >= 0 && ny < grid.count && nx >= 0 && nx < grid[ny].count) {
            if (grid[ny][nx] && (nx != x || ny != y)) {
              onNeighbors = onNeighbors + 1
            }
          }
        }
      }
      if (grid[y][x]) {
        row.add(onNeighbors == 2 || onNeighbors == 3)
      } else {
        row.add(onNeighbors == 3)
      }
    }
    newGrid.add(row)
  }
  grid = newGrid
}

var onCount = 0
for (y in 0..grid.count - 1) {
  for (x in 0..grid[y].count - 1) {
    if (grid[y][x]) {
      onCount = onCount + 1
    }
  }
}

System.print(onCount)