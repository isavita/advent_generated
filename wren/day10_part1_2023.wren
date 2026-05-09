import "io" for File

var content = File.read("input.txt")
if (content == null || content.trim() == "") {
  System.print("Error: Could not read file or file is empty.")
  return
}
content = content.trim()
var grid = content.split("\n")
var R = grid.count
if (R == 0) {
  System.print("Error: Empty grid.")
  return
}
var C = grid[0].count
if (C == 0) {
  System.print("Error: Empty row.")
  return
}

var start = null
for (r in 0...R) {
  var line = grid[r]
  for (c in 0...C) {
    if (line[c] == "S") {
      start = [r, c]
      break
    }
  }
  if (start) break
}
if (!start) {
  System.print("Error: Start not found.")
  return
}

var distances = []
for (r in 0...R) {
  var row = []
  for (c in 0...C) {
    row.add(-1)
  }
  distances.add(row)
}

var queue = []
var head = 0

var sr = start[0]
var sc = start[1]

var dirs = [
  [-1, 0, ["|", "7", "F"]],
  [1, 0, ["|", "L", "J"]],
  [0, -1, ["-", "L", "F"]],
  [0, 1, ["-", "J", "7"]]
]

for (dir in dirs) {
  var dr = dir[0]
  var dc = dir[1]
  var validPipes = dir[2]
  var nr = sr + dr
  var nc = sc + dc
  if (nr >= 0 && nr < R && nc >= 0 && nc < C) {
    var ch = grid[nr][nc]
    if (validPipes.contains(ch)) {
      if (distances[nr][nc] == -1) {
        distances[nr][nc] = 1
        queue.add([nr, nc])
      }
    }
  }
}

var maxDist = 0
while (head < queue.count) {
  var curr = queue[head]
  head = head + 1
  var r = curr[0]
  var c = curr[1]
  var d = distances[r][c]
  if (d > maxDist) maxDist = d
  var pipe = grid[r][c]
  var neighbors = []
  if (pipe == "|") {
    neighbors = [[r-1, c], [r+1, c]]
  } else if (pipe == "-") {
    neighbors = [[r, c-1], [r, c+1]]
  } else if (pipe == "L") {
    neighbors = [[r-1, c], [r, c+1]]
  } else if (pipe == "J") {
    neighbors = [[r-1, c], [r, c-1]]
  } else if (pipe == "7") {
    neighbors = [[r+1, c], [r, c-1]]
  } else if (pipe == "F") {
    neighbors = [[r+1, c], [r, c+1]]
  } else {
    continue
  }
  for (neighbor in neighbors) {
    var nr = neighbor[0]
    var nc = neighbor[1]
    if (nr >= 0 && nr < R && nc >= 0 && nc < C && distances[nr][nc] == -1 && grid[nr][nc] != ".") {
      distances[nr][nc] = d + 1
      queue.add([nr, nc])
    }
  }
}

System.print(maxDist)