
import "io" for File

var content = File.read("input.txt")
var lines = content.replace("\r", "").split("\n")

var g = {}
var ix = {}
var xx = {}
var iy = {}
var xy = {}

var r = 0
var parsingGrid = true
var pathStr = ""

for (line in lines) {
    if (parsingGrid) {
        if (line == "") {
            if (r > 0) parsingGrid = false
            continue
        }
        r = r + 1
        var n = line.count
        for (c in 1..n) {
            var v = line[c-1]
            if (v != " ") {
                g[r * 1000 + c] = v
                if (!ix.containsKey(r)) ix[r] = c
                xx[r] = c
                if (!iy.containsKey(c)) iy[c] = r
                xy[c] = r
            }
        }
    } else {
        if (line != "") pathStr = pathStr + line
    }
}

var path = []
var num = ""
for (char in pathStr) {
    if (char == "R" || char == "L") {
        if (num != "") {
            path.add(Num.fromString(num))
            num = ""
        }
        path.add(char)
    } else {
        num = num + char
    }
}
if (num != "") path.add(Num.fromString(num))

var y = 1
var x = 1
while (g[1000 + x] != ".") x = x + 1

var f = 0
var dx = [1, 0, -1, 0]
var dy = [0, 1, 0, -1]

for (m in path) {
    if (m == "R") {
        f = (f + 1) % 4
    } else if (m == "L") {
        f = (f + 3) % 4
    } else {
        for (i in 0...m) {
            var nx = x + dx[f]
            var ny = y + dy[f]
            if (!g.containsKey(ny * 1000 + nx)) {
                if (f == 0) {
                    nx = ix[y]
                } else if (f == 2) {
                    nx = xx[y]
                } else if (f == 1) {
                    ny = iy[x]
                } else if (f == 3) {
                    ny = xy[x]
                }
            }
            if (g[ny * 1000 + nx] == "#") break
            x = nx
            y = ny
        }
    }
}

System.print(1000 * y + 4 * x + f)
