
import "io" for File

var grid = File.read("input.txt").split("\n").map { |s| s.trim() }.where { |s| s.count > 0 }.toList
var rows = grid.count
var word = "XMAS"
var dirs = [
  [0, 1], [0, -1], [1, 0], [-1, 0],
  [1, 1], [1, -1], [-1, -1], [-1, 1]
]
var count = 0

for (r in 0...rows) {
  var cols = grid[r].count
  for (c in 0...cols) {
    if (grid[r][c] == "X") {
      for (d in dirs) {
        var dr = d[0]
        var dc = d[1]
        var ok = true
        for (i in 0...4) {
          var rr = r + i * dr
          var cc = c + i * dc
          if (rr < 0 || rr >= rows || cc < 0 || cc >= grid[rr].count || grid[rr][cc] != word[i]) {
            ok = false
            break
          }
        }
        if (ok) count = count + 1
      }
    }
  }
}

System.print(count)
