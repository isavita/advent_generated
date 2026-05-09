import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
var grid = []
for (line in lines) {
  if (line.trim() == "") continue
  var row = []
  for (ch in line) {
    row.add(Num.fromString(ch))
  }
  grid.add(row)
}
if (grid.count == 0) {
  System.print("Error: empty grid")
  return
}
var rows = grid.count
var cols = grid[0].count
var paths = []
for (r in 0...rows) {
  var row = []
  for (c in 0...cols) {
    row.add(0)
  }
  paths.add(row)
}
var cellsByHeight = []
for (i in 0..9) {
  cellsByHeight.add([])
}
for (r in 0...rows) {
  for (c in 0...cols) {
    var h = grid[r][c]
    cellsByHeight[h].add([r, c])
  }
}
for (h in 9..0) {
  for (cell in cellsByHeight[h]) {
    var r = cell[0]
    var c = cell[1]
    if (h == 9) {
      paths[r][c] = 1
    } else {
      var sum = 0
      var dirs = [[0,1], [0,-1], [1,0], [-1,0]]
      for (dir in dirs) {
        var nr = r + dir[0]
        var nc = c + dir[1]
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
          if (grid[nr][nc] == h + 1) {
            sum = sum + paths[nr][nc]
          }
        }
      }
      paths[r][c] = sum
    }
  }
}
var total = 0
for (cell in cellsByHeight[0]) {
  var r = cell[0]
  var c = cell[1]
  total = total + paths[r][c]
}
System.print(total)