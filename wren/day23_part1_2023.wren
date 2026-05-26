
import "io" for File

var content = File.read("input.txt")
var grid = []
var row = []
for (b in content.bytes) {
    if (b == 10) {
        if (row.count > 0) {
            grid.add(row)
            row = []
        }
    } else if (b != 13) {
        row.add(b)
    }
}
if (row.count > 0) grid.add(row)

var R = grid.count
var C = grid[0].count

var gridBytes = List.filled(R * C, 0)
for (r in 0...R) {
    for (c in 0...C) {
        gridBytes[r * C + c] = grid[r][c]
    }
}

var DR = [-1, 1, 0, 0]
var DC = [0, 0, -1, 1]

var isValidBasic = Fn.new {|r, c|
    if (r < 0 || r >= R || c < 0 || c >= C) return false
    return gridBytes[r * C + c] != 35
}

var isValid = Fn.new {|r, c, dr, dc|
    if (!isValidBasic.call(r, c)) return false
    var ch = gridBytes[r * C + c]
    if (ch == 46) return true
    if (ch == 94) return dr == -1
    if (ch == 118) return dr == 1
    if (ch == 60) return dc == -1
    if (ch == 62) return dc == 1
    return false
}

var nodeId = {}
var nodes = []

var addNode = Fn.new {|r, c|
    var key = r * C + c
    if (!nodeId.containsKey(key)) {
        nodeId[key] = nodes.count
        nodes.add([r, c])
    }
}

addNode.call(0, 1)
addNode.call(R - 1, C - 2)

for (r in 0...R) {
    for (c in 0...C) {
        if (gridBytes[r * C + c] == 46) {
            var neighs = 0
            for (i in 0...4) {
                if (isValidBasic.call(r + DR[i], c + DC[i])) neighs = neighs + 1
            }
            if (neighs > 2) {
                addNode.call(r, c)
            }
        }
    }
}

var adj = List.filled(nodes.count, null)
for (i in 0...nodes.count) adj[i] = []

var visitedGrid = List.filled(R * C, 0)

for (u in 0...nodes.count) {
    var startPos = nodes[u]
    var qr = [startPos[0]]
    var qc = [startPos[1]]
    var qd = [0]
    var qhead = 0
    
    var marker = u + 1
    visitedGrid[startPos[0] * C + startPos[1]] = marker
    
    while (qhead < qr.count) {
        var r = qr[qhead]
        var c = qc[qhead]
        var d = qd[qhead]
        qhead = qhead + 1
        
        var key = r * C + c
        if ((r != startPos[0] || c != startPos[1]) && nodeId.containsKey(key)) {
            var v = nodeId[key]
            adj[u].add([v, d])
            continue
        }
        
        for (i in 0...4) {
            var nr = r + DR[i]
            var nc = c + DC[i]
            if (isValid.call(nr, nc, DR[i], DC[i])) {
                var nkey = nr * C + nc
                if (visitedGrid[nkey] != marker) {
                    visitedGrid[nkey] = marker
                    qr.add(nr)
                    qc.add(nc)
                    qd.add(d + 1)
                }
            }
        }
    }
}

var maxD = 0
var visitedNodes = List.filled(nodes.count, false)

var dfs
dfs = Fn.new {|u, curD|
    if (u == 1) {
        if (curD > maxD) maxD = curD
        return
    }
    visitedNodes[u] = true
    for (edge in adj[u]) {
        var v = edge[0]
        var w = edge[1]
        if (!visitedNodes[v]) {
            dfs.call(v, curD + w)
        }
    }
    visitedNodes[u] = false
}

dfs.call(0, 0)
System.print(maxD)
