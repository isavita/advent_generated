
import "io" for File

var dists = {}
var cities = []
var raw = File.read("input.txt").trim()
if (raw.isEmpty) return

for (line in raw.split("\n")) {
    var parts = line.split(" = ")
    var d = Num.fromString(parts[1].trim())
    var pair = parts[0].split(" to ")
    var c1 = pair[0].trim()
    var c2 = pair[1].trim()
    for (c in [c1, c2]) {
        if (!dists.containsKey(c)) {
            dists[c] = {}
            cities.add(c)
        }
    }
    dists[c1][c2] = d
    dists[c2][c1] = d
}

var minD = 1/0
var maxD = 0

var walk
walk = Fn.new { |curr, visited, d|
    if (visited.count == cities.count) {
        if (d < minD) minD = d
        if (d > maxD) maxD = d
        return
    }
    for (next in cities) {
        if (!visited.containsKey(next)) {
            visited[next] = true
            walk.call(next, visited, curr == "" ? 0 : d + dists[curr][next])
            visited.remove(next)
        }
    }
}

walk.call("", {}, 0)

System.print("Shortest distance: %(minD)")
System.print("Longest distance: %(maxD)")
