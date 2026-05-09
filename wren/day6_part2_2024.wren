
import "io" for File

var DIRS = [[0, -1], [1, 0], [0, 1], [-1, 0]]

var loops = Fn.new {|grid, startX, startY, startDir|
  var h = grid.count
  var w = grid[0].count
  var x = startX
  var y = startY
  var dirIdx = startDir
  var seen = {}
  while (true) {
    var key = "%(x),%(y),%(dirIdx)"
    if (seen.containsKey(key)) return true
    seen[key] = true
    var dx = DIRS[dirIdx][0]
    var dy = DIRS[dirIdx][1]
    var nx = x + dx
    var ny = y + dy
    if (nx < 0 || nx >= w || ny < 0 || ny >= h) return false
    if (grid[ny][nx] == "#") {
      dirIdx = (dirIdx + 1) % 4
    } else {
      x = nx
      y = ny
    }
  }
}

var findLoopPositions = Fn.new {|inputStr|
  var lines = inputStr.trim().split("\n")
  if (lines.count == 0) return 0
  var grid = []
  for (line in lines) {
    var row = []
    for (i in 0...line.count) {
      row.add(line[i])
    }
    grid.add(row)
  }
  var h = grid.count
  var w = grid[0].count
  var startX = -1
  var startY = -1
  var startDir = -1
  var startInfo = null
  for (r in 0...h) {
    for (c in 0...w) {
      var ch = grid[r][c]
      if (ch == "^") {
        startInfo = [c, r, 0]
        break
      } else if (ch == ">") {
        startInfo = [c, r, 1]
        break
      } else if (ch == "v") {
        startInfo = [c, r, 2]
        break
      } else if (ch == "<") {
        startInfo = [c, r, 3]
        break
      }
    }
    if (startInfo) break
  }
  if (!startInfo) return 0
  startX = startInfo[0]
  startY = startInfo[1]
  startDir = startInfo[2]
  grid[startY][startX] = "."
  var count = 0
  for (r in 0...h) {
    for (c in 0...w) {
      if (r == startY && c == startX) continue
      if (grid[r][c] == ".") {
        grid[r][c] = "#"
        if (loops.call(grid, startX, startY, startDir)) count = count + 1
        grid[r][c] = "."
      }
    }
  }
  return count
}

var input = File.read("input.txt")
System.print(findLoopPositions.call(input))
