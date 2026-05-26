
import "io" for File

var clean = Fn.new {|s| s.endsWith(",") ? s[0...-1] : s }

var lines = File.read("input.txt").split("\n").map {|l| l.trim() }.where {|l| l.bytes.count > 0 }.toList

var rates = {}
var adj = {}
var valves = []

for (line in lines) {
    var parts = line.split(" ")
    var name = parts[1]
    valves.add(name)
    var rate = Num.fromString(parts[4].split("=")[1].split(";")[0])
    rates[name] = rate
    
    var vIdx = -1
    for (i in 5...parts.count) {
        if (parts[i] == "valve" || parts[i] == "valves") {
            vIdx = i
            break
        }
    }
    var neighbors = []
    for (i in vIdx + 1...parts.count) {
        neighbors.add(clean.call(parts[i]))
    }
    adj[name] = neighbors
}

var dist = {}
var inf = 999999
for (u in valves) {
    dist[u] = {}
    for (v in valves) {
        dist[u][v] = (u == v) ? 0 : inf
    }
}
for (u in valves) {
    for (v in adj[u]) {
        dist[u][v] = 1
    }
}

for (k in valves) {
    for (i in valves) {
        for (j in valves) {
            var d = dist[i][k] + dist[k][j]
            if (d < dist[i][j]) {
                dist[i][j] = d
            }
        }
    }
}

var useful = []
for (v in rates.keys) {
    if (rates[v] > 0) {
        useful.add(v)
    }
}
var usefulCount = useful.count
var bitVal = {}
var p2 = 1
for (v in useful) {
    bitVal[v] = p2
    p2 = p2 * 2
}
var totalMasks = p2

var dfs
dfs = Fn.new {|u, time, mask, pressure, results|
    if (pressure > results[mask]) {
        results[mask] = pressure
    }
    for (i in 0...usefulCount) {
        var v = useful[i]
        var m = bitVal[v]
        if ((mask & m) == 0) {
            var d = dist[u][v]
            var rem = time - d - 1
            if (rem > 0) {
                dfs.call(v, rem, mask + m, pressure + (rem * rates[v]), results)
            }
        }
    }
}

var res1 = List.filled(totalMasks, 0)
dfs.call("AA", 30, 0, 0, res1)
var max1 = 0
for (p in res1) {
    if (p > max1) max1 = p
}
System.print("Part 1: %(max1)")

var res2 = List.filled(totalMasks, 0)
dfs.call("AA", 26, 0, 0, res2)

for (i in 0...usefulCount) {
    var m = 1 << i
    for (mask in 0...totalMasks) {
        if ((mask & m) != 0) {
            var prev = mask - m
            if (res2[prev] > res2[mask]) {
                res2[mask] = res2[prev]
            }
        }
    }
}

var max2 = 0
for (mask in 0...totalMasks) {
    var complement = (totalMasks - 1) - mask
    if (res2[mask] + res2[complement] > max2) {
        max2 = res2[mask] + res2[complement]
    }
}
System.print("Part 2: %(max2)")
