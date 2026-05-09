
import "io" for File

var content = File.read("input.txt")
if (content == null) {
  System.print("Error: Input file 'input.txt' not found.")
  return
}

content = content.trim().replace("\r", "")
var lines = content.split("\n")

if (lines.count == 0 || lines[0].count == 0) {
  System.print(0)
  return
}

var grid = lines
var rows = grid.count
var cols = grid[0].count

var visited = []
for (r in 0...rows) {
  var row = []
  for (c in 0...cols) row.add(false)
  visited.add(row)
}

var totalPrice = 0
var dr = [-1, 1, 0, 0]
var dc = [0, 0, -1, 1]

var findRegion = Fn.new {|startR, startC, grid, visited|
  var rows = grid.count
  var cols = grid[0].count
  if (startR < 0 || startR >= rows || startC < 0 || startC >= cols) return [0, 0]
  if (visited[startR][startC]) return [0, 0]

  var plantType = grid[startR][startC]
  var area = 0
  var perimeter = 0
  var queue = [[startR, startC]]
  visited[startR][startC] = true
  var head = 0

  while (head < queue.count) {
    var curr = queue[head]
    head = head + 1
    var r = curr[0]
    var c = curr[1]
    area = area + 1

    for (i in 0..3) {
      var nr = r + dr[i]
      var nc = c + dc[i]
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        if (grid[nr][nc] == plantType) {
          if (!visited[nr][nc]) {
            visited[nr][nc] = true
            queue.add([nr, nc])
          }
        } else {
          perimeter = perimeter + 1
        }
      } else {
        perimeter = perimeter + 1
      }
    }
  }
  return [area, perimeter]
}

for (r in 0...rows) {
  for (c in 0...cols) {
    if (!visited[r][c]) {
      var res = findRegion.call(r, c, grid, visited)
      var area = res[0]
      var perimeter = res[1]
      if (area > 0) totalPrice = totalPrice + area * perimeter
    }
  }
}

System.print(totalPrice)
