
import "io" for File

var grid = File.read("input.txt").replace("\r", "").split("\n").where { |l| l != "" }.toList
var h = grid.count
var w = grid[0].count

var x = 0
var y = 0
var dir = 0
var dirs = ["^", ">", "v", "<"]

for (i in 0...h) {
  for (j in 0...w) {
    var idx = dirs.indexOf(grid[i][j])
    if (idx >= 0) {
      x = j
      y = i
      dir = idx
    }
  }
}

var dx = [0, 1, 0, -1]
var dy = [-1, 0, 1, 0]
var visited = {}
visited["%(x),%(y)"] = true

while (true) {
  var nx = x + dx[dir]
  var ny = y + dy[dir]
  if (nx < 0 || nx >= w || ny < 0 || ny >= h) break
  if (grid[ny][nx] == "#") {
    dir = (dir + 1) % 4
  } else {
    x = nx
    y = ny
    visited["%(x),%(y)"] = true
  }
}

System.print(visited.count)
