
import "io" for File

var lines = File.read("input.txt").split("\n")
if (lines.count > 0 && lines[-1] == "") lines.removeAt(-1)

var h = lines.count
var w = lines[0].count
if (lines[0].endsWith("\r")) w = w - 1

var sx = 0
var sy = 0
var walls = List.filled(w * h, false)

for (y in 0...h) {
    var line = lines[y]
    for (x in 0...w) {
        var c = line[x]
        if (c == "#") {
            walls[y * w + x] = true
        } else if (c == "S") {
            sx = x
            sy = y
        }
    }
}

var dist = List.filled(w * h, -1)
var start = sy * w + sx
dist[start] = 0

var q = [start]
var head = 0
var dx = [1, -1, 0, 0]
var dy = [0, 0, 1, -1]

while (head < q.count) {
    var curr = q[head]
    head = head + 1
    var cd = dist[curr]
    if (cd < 64) {
        var cx = curr % w
        var cy = (curr / w).floor
        for (i in 0...4) {
            var nx = cx + dx[i]
            var ny = cy + dy[i]
            if (nx >= 0 && nx < w && ny >= 0 && ny < h) {
                var nidx = ny * w + nx
                if (!walls[nidx] && dist[nidx] == -1) {
                    dist[nidx] = cd + 1
                    q.add(nidx)
                }
            }
        }
    }
}

var ans = 0
for (d in dist) {
    if (d != -1 && d % 2 == 0) {
        ans = ans + 1
    }
}
System.print(ans)
