
import "io" for File

var content = File.read("input.txt")
content = content.trimEnd()
var grid = content.split("\n")
var h = grid.count
var w = grid[0].count

var antennas = {}

for (y in 0...h) {
  var line = grid[y]
  for (x in 0...w) {
    var c = line[x]
    if (c != ".") {
      if (!antennas.containsKey(c)) {
        antennas[c] = []
      }
      antennas[c].add([y, x])
    }
  }
}

var antinodesGrid = []
for (i in 0...h) {
  antinodesGrid.add(List.filled(w, false))
}

for (freq in antennas.keys) {
  var coords = antennas[freq]
  var n = coords.count
  for (i in 0...n) {
    for (j in i+1...n) {
      var A = coords[i]
      var B = coords[j]
      var p1y = 2*A[0] - B[0]
      var p1x = 2*A[1] - B[1]
      var p2y = 2*B[0] - A[0]
      var p2x = 2*B[1] - A[1]
      if (p1y >= 0 && p1y < h && p1x >= 0 && p1x < w) {
        antinodesGrid[p1y][p1x] = true
      }
      if (p2y >= 0 && p2y < h && p2x >= 0 && p2x < w) {
        antinodesGrid[p2y][p2x] = true
      }
    }
  }
}

var count = 0
for (row in antinodesGrid) {
  for (val in row) {
    if (val) count = count + 1
  }
}
System.print(count)
