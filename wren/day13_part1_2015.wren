
import "io" for File

var h = {}
var people = []
var lines = File.read("input.txt").split("\n")
for (line in lines) {
    var w = line.trim().split(" ")
    if (w.count < 11) continue
    var p1 = w[0]
    var p2 = w[10][0...-1]
    h[p1 + "_" + p2] = Num.fromString(w[3]) * (w[2] == "gain" ? 1 : -1)
    if (!people.contains(p1)) people.add(p1)
}

var n = people.count
var max = -999999999
var order = List.filled(n, "")
var used = List.filled(n, false)

var dfs
dfs = Fn.new {|depth|
    if (depth == n) {
        var total = 0
        for (i in 0...n) {
            var p1 = order[i]
            var p2 = order[(i + 1) % n]
            total = total + h[p1 + "_" + p2] + h[p2 + "_" + p1]
        }
        if (total > max) max = total
        return
    }
    for (i in 0...n) {
        if (!used[i]) {
            used[i] = true
            order[depth] = people[i]
            dfs.call(depth + 1)
            used[i] = false
        }
    }
}

dfs.call(0)
System.print(max)
