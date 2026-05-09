
import "io" for File

var findReachableNines = Fn.new { |grid, startCoord|
  var rows = grid.count
  var cols = grid[0].count
  var queue = [startCoord]
  var visited = List.filled(rows, null)
  for (i in 0...rows) {
    visited[i] = List.filled(cols, false)
  }
  visited[startCoord[0]][startCoord[1]] = true
  var count = 0
  var dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]]
  while (queue.count > 0) {
    var current = queue.removeAt(0)
    var r = current[0]
    var c = current[1]
    var height = grid[r][c]
    if (height == 9) {
      count = count + 1
    }
    for (dir in dirs) {
      var nr = r + dir[0]
      var nc = c + dir[1]
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        if (grid[nr][nc] == height + 1 && !visited[nr][nc]) {
          visited[nr][nc] = true
          queue.add([nr, nc])
        }
      }
    }
  }
  return count
}

var main = Fn.new {
  var content = File.read("input.txt")
  var lines = content.split("\n")
  var grid = []
  for (line in lines) {
    var trimmed = line.trim()
    if (trimmed != "") {
      var row = []
      for (i in 0...trimmed.count) {
        var ch = trimmed[i]
        var num = Num.fromString(ch)
        if (num != null) {
          row.add(num)
        }
      }
      if (row.count > 0) {
        grid.add(row)
      }
    }
  }

  if (grid.count == 0 || grid[0].count == 0) {
    System.print("Error: Input grid is empty or invalid.")
    return
  }

  var rows = grid.count
  var cols = grid[0].count

  var trailheads = []
  for (r in 0...rows) {
    for (c in 0...cols) {
      if (grid[r][c] == 0) {
        trailheads.add([r, c])
      }
    }
  }

  var totalScore = 0
  for (trailhead in trailheads) {
    totalScore = totalScore + findReachableNines.call(grid, trailhead)
  }

  System.print(totalScore)
}

main.call()
