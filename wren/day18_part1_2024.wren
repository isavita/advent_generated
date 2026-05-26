
import "io" for File

var gridSize = 71
var maxLines = 1024

var corrupted = List.filled(gridSize * gridSize, false)
var content = File.read("input.txt")
var lines = content.split("\n")
var count = 0
for (line in lines) {
  var parts = line.trim().split(",")
  if (parts.count < 2) continue
  var x = Num.fromString(parts[0])
  var y = Num.fromString(parts[1])
  corrupted[y * gridSize + x] = true
  count = count + 1
  if (count == maxLines) break
}

var visited = List.filled(gridSize * gridSize, false)
var q = [[0, 0, 0]]
visited[0] = true
var head = 0
var steps = -1

var dx = [0, 0, 1, -1]
var dy = [1, -1, 0, 0]

while (head < q.count) {
  var curr = q[head]
  head = head + 1
  var x = curr[0]
  var y = curr[1]
  var s = curr[2]

  if (x == gridSize - 1 && y == gridSize - 1) {
    steps = s
    break
  }

  for (i in 0...4) {
    var nx = x + dx[i]
    var ny = y + dy[i]
    if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize) {
      var idx = ny * gridSize + nx
      if (!corrupted[idx] && !visited[idx]) {
        visited[idx] = true
        q.add([nx, ny, s + 1])
      }
    }
  }
}

System.print(steps)
