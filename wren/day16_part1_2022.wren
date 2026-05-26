
import "io" for File

var lines = File.read("input.txt").trim().split("\n")

var names = []
var nameToIdx = {}
var getIdx = Fn.new {|name|
    var idx = nameToIdx[name]
    if (idx == null) {
        idx = names.count
        nameToIdx[name] = idx
        names.add(name)
    }
    return idx
}

var rawFlows = {}
var rawDists = {}

for (line in lines) {
    if (line == "") continue
    var parts = line.split(" ")
    var v = parts[1]
    
    var rateParts = parts[4].split("=")
    var rateStr = rateParts[1]
    if (rateStr.endsWith(";")) {
        rateStr = rateStr[0...-1]
    }
    var rate = Num.fromString(rateStr)
    rawFlows[v] = rate
    
    rawDists[v] = {}
    rawDists[v][v] = 0
    
    for (i in 9...parts.count) {
        var t = parts[i]
        if (t.endsWith(",")) {
            t = t[0...-1]
        }
        rawDists[v][t] = 1
    }
}

var nValves = rawFlows.count
var flow = List.filled(nValves, 0)
var dist = List.filled(nValves, null)
for (i in 0...nValves) {
    dist[i] = List.filled(nValves, 999999)
    dist[i][i] = 0
}

for (v in rawFlows.keys) {
    var uIdx = getIdx.call(v)
    flow[uIdx] = rawFlows[v]
    for (t in rawDists[v].keys) {
        var vIdx = getIdx.call(t)
        dist[uIdx][vIdx] = rawDists[v][t]
    }
}

for (k in 0...nValves) {
    for (i in 0...nValves) {
        for (j in 0...nValves) {
            var d = dist[i][k] + dist[k][j]
            if (d < dist[i][j]) {
                dist[i][j] = d
            }
        }
    }
}

var startIdx = nameToIdx["AA"]
var useful = []
for (i in 0...nValves) {
    if (flow[i] > 0) {
        useful.add(i)
    }
}

var max_p = 0
var visited = List.filled(nValves, false)

var solve
solve = Fn.new {|curr, time, score|
    if (score > max_p) max_p = score
    for (target in useful) {
        if (!visited[target]) {
            var cost = dist[curr][target] + 1
            if (time > cost) {
                visited[target] = true
                solve.call(target, time - cost, score + (time - cost) * flow[target])
                visited[target] = false
            }
        }
    }
}

solve.call(startIdx, 30, 0)
System.print(max_p)
