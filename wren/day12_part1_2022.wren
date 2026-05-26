
import "io" for File

var grid = []
for (line in File.read("input.txt").split("\n")) {
    var trimmed = line.trim()
    if (trimmed.count > 0) {
        grid.add(trimmed.bytes.toList)
    }
}

var rows = grid.count
var cols = grid[0].count

var sx = -1
var sy = -1
var ex = -1
var ey = -1

for (r in 0...rows) {
    for (c in 0...cols) {
        var v = grid[r][c]
        if (v == 83) {
            sx = c
            sy = r
            grid[r][c] = 97
        } else if (v == 69) {
            ex = c
            ey = r
            grid[r][c] = 122
        }
    }
}

var dist = List.filled(rows * cols, -1)
var q = [ex + ey * cols]
dist[ex + ey * cols] = 0

var dx = [0, 0, 1, -1]
var dy = [1, -1, 0, 0]

var head = 0
while (head < q.count) {
    var curr = q[head]
    head = head + 1
    
    var cx = curr % cols
    var cy = ((curr - cx) / cols).floor
    var d = dist[curr]
    var ch = grid[cy][cx]

    for (i in 0...4) {
        var nx = cx + dx[i]
        var ny = cy + dy[i]
        if (nx >= 0 && nx < cols && ny >= 0 && ny < rows) {
            var nIdx = nx + ny * cols
            var nh = grid[ny][nx]
            if (ch - nh <= 1) {
                if (dist[nIdx] == -1) {
                    dist[nIdx] = d + 1
                    q.add(nIdx)
                }
            }
        }
    }
}

System.print(dist[sx + sy * cols])
