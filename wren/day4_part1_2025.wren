
import "io" for File

// read non‑empty lines
var lines = File.read("input.txt").split("\n").where { |l| l.count > 0 }.toList

var rows = lines.count
var cols = rows > 0 ? lines[0].count : 0

// 8 neighbour offsets
var dx = [-1, -1, -1, 0, 0, 1, 1, 1]
var dy = [-1, 0, 1, -1, 1, -1, 0, 1]

var ans = 0
for (y in 0...rows) {
    var row = lines[y]
    for (x in 0...cols) {
        if (row[x] != "@") continue

        var cnt = 0
        for (d in 0...8) {
            var nx = x + dx[d]
            var ny = y + dy[d]
            if (nx >= 0 && nx < cols && ny >= 0 && ny < rows) {
                if (lines[ny][nx] == "@") cnt = cnt + 1
            }
        }

        if (cnt < 4) ans = ans + 1
    }
}

System.print(ans)
