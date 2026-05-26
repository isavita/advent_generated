
import "io" for File

var content = File.read("input.txt").replace("\r", "")
var lines = content.split("\n")
if (lines.count > 0 && lines[-1] == "") lines.removeAt(-1)

var R = lines.count
var C = lines[0].count

var grid = List.filled(R * C, 0)
var end = -1

for (r in 0...R) {
    var row = lines[r]
    for (c in 0...C) {
        var char = row[c]
        var idx = r * C + c
        if (char == "S") {
            grid[idx] = 0
        } else if (char == "E") {
            grid[idx] = 25
            end = idx
        } else {
            grid[idx] = char.codePoints[0] - 97
        }
    }
}

var dist = List.filled(R * C, -1)
var q = [end]
dist[end] = 0

var qHead = 0
var dr = [1, -1, 0, 0]
var dc = [0, 0, 1, -1]

while (qHead < q.count) {
    var curr = q[qHead]
    qHead = qHead + 1
    
    var r = (curr / C).floor
    var c = curr % C
    
    if (grid[curr] == 0) {
        System.print(dist[curr])
        break
    }
    
    for (i in 0...4) {
        var nr = r + dr[i]
        var nc = c + dc[i]
        if (nr >= 0 && nr < R && nc >= 0 && nc < C) {
            var nidx = nr * C + nc
            if (dist[nidx] == -1) {
                if (grid[curr] - grid[nidx] <= 1) {
                    dist[nidx] = dist[curr] + 1
                    q.add(nidx)
                }
            }
        }
    }
}
