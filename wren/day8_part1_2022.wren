
import "io" for File

var content = File.read("input.txt")
var lines = content.replace("\r", "").trim().split("\n")
var grid = lines.map { |line| line.bytes.map { |b| b - 48 }.toList }.toList
var R = grid.count
var C = grid[0].count

var dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]]
var count = 0

for (r in 0...R) {
  for (c in 0...C) {
    var isVisible = false
    for (d in dirs) {
      var dr = d[0]
      var dc = d[1]
      var nr = r + dr
      var nc = c + dc
      var ok = true
      while (nr >= 0 && nr < R && nc >= 0 && nc < C) {
        if (grid[nr][nc] >= grid[r][c]) {
          ok = false
          break
        }
        nr = nr + dr
        nc = nc + dc
      }
      if (ok) {
        isVisible = true
        break
      }
    }
    if (isVisible) count = count + 1
  }
}

System.print(count)
