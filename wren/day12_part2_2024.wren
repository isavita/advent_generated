
import "io" for File

var lines = File.read("input.txt").replace("\r", "").split("\n")
if (lines.count > 0 && lines[lines.count - 1] == "") {
  lines.removeAt(lines.count - 1)
}

var grid = []
for (line in lines) {
  if (line != "") {
    var row = []
    for (i in 0...line.count) {
      row.add(line[i])
    }
    grid.add(row)
  }
}

var height = grid.count
var width = height > 0 ? grid[0].count : 0

var visited = List.filled(height, null)
var in_region = List.filled(height, null)
for (i in 0...height) {
  visited[i] = List.filled(width, false)
  in_region[i] = List.filled(width, false)
}

var get_plant = Fn.new { |r, c|
  if (r < 0 || r >= height || c < 0 || c >= width) return "."
  return grid[r][c]
}

var total1 = 0
var total2 = 0

var dr = [-1, 1, 0, 0]
var dc = [0, 0, -1, 1]

for (r0 in 0...height) {
  for (c0 in 0...width) {
    if (visited[r0][c0]) continue

    var plant = grid[r0][c0]
    var q_r = [r0]
    var q_c = [c0]
    var qh = 0
    visited[r0][c0] = true

    var region_r = []
    var region_c = []
    var area = 0
    var perimeter = 0

    while (qh < q_r.count) {
      var r = q_r[qh]
      var c = q_c[qh]
      qh = qh + 1

      area = area + 1
      region_r.add(r)
      region_c.add(c)

      for (i in 0...4) {
        var nr = r + dr[i]
        var nc = c + dc[i]
        var n_plant = get_plant.call(nr, nc)
        if (n_plant != plant) {
          perimeter = perimeter + 1
        } else {
          if (!visited[nr][nc]) {
            visited[nr][nc] = true
            q_r.add(nr)
            q_c.add(nc)
          }
        }
      }
    }

    total1 = total1 + area * perimeter

    for (i in 0...region_r.count) {
      var r = region_r[i]
      var c = region_c[i]
      in_region[r][c] = true
    }

    var top = 0
    var bottom = 0
    var left = 0
    var right = 0
    var top_adj = 0
    var bottom_adj = 0
    var left_adj = 0
    var right_adj = 0

    for (i in 0...region_r.count) {
      var r = region_r[i]
      var c = region_c[i]

      if (get_plant.call(r-1, c) != plant) top = top + 1
      if (get_plant.call(r+1, c) != plant) bottom = bottom + 1
      if (get_plant.call(r, c-1) != plant) left = left + 1
      if (get_plant.call(r, c+1) != plant) right = right + 1

      if (c + 1 < width && in_region[r][c+1]) {
        if (get_plant.call(r-1, c) != plant && get_plant.call(r-1, c+1) != plant) {
          top_adj = top_adj + 1
        }
        if (get_plant.call(r+1, c) != plant && get_plant.call(r+1, c+1) != plant) {
          bottom_adj = bottom_adj + 1
        }
      }
      if (r + 1 < height && in_region[r+1][c]) {
        if (get_plant.call(r, c-1) != plant && get_plant.call(r+1, c-1) != plant) {
          left_adj = left_adj + 1
        }
        if (get_plant.call(r, c+1) != plant && get_plant.call(r+1, c+1) != plant) {
          right_adj = right_adj + 1
        }
      }
    }

    var sides = (top - top_adj) + (bottom - bottom_adj) + (left - left_adj) + (right - right_adj)
    total2 = total2 + area * sides

    for (i in 0...region_r.count) {
      var r = region_r[i]
      var c = region_c[i]
      in_region[r][c] = false
    }
  }
}

System.print("Part 1 Total Price: %(total1)")
System.print("Part 2 Total Price: %(total2)")
