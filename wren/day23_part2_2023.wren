
import "io" for File

class Day23 {
    static run() {
        var lines = File.read("input.txt").split("\n").map { |s| s.trim() }.where { |s| s.bytes.count > 0 }.toList
        var h = lines.count
        var w = lines[0].count
        var grid = lines.map { |line| line.bytes.toList }.toList
        
        var sy = 0
        var sx = -1
        var ey = h - 1
        var ex = -1
        for (x in 0...w) {
            if (grid[0][x] == 46) sx = x
            if (grid[h - 1][x] == 46) ex = x
        }
        
        var dr = [0, 0, 1, -1]
        var dc = [1, -1, 0, 0]
        
        var isNode = List.filled(h, null)
        for (r in 0...h) {
            isNode[r] = List.filled(w, false)
        }
        
        var nodeId = 0
        var nodeToId = {}
        var idToR = []
        var idToC = []
        
        for (r in 0...h) {
            for (c in 0...w) {
                if (grid[r][c] == 35) continue
                var deg = 0
                for (i in 0...4) {
                    var nr = r + dr[i]
                    var nc = c + dc[i]
                    if (nr >= 0 && nr < h && nc >= 0 && nc < w && grid[nr][nc] != 35) {
                        deg = deg + 1
                    }
                }
                if (deg > 2 || r == 0 || r == h - 1) {
                    isNode[r][c] = true
                    var key = "%(r),%(c)"
                    nodeToId[key] = nodeId
                    idToR.add(r)
                    idToC.add(c)
                    nodeId = nodeId + 1
                }
            }
        }
        
        var adjV = List.filled(nodeId, null)
        var adjW = List.filled(nodeId, null)
        for (i in 0...nodeId) {
            adjV[i] = []
            adjW[i] = []
        }
        
        for (uid in 0...nodeId) {
            var r = idToR[uid]
            var c = idToC[uid]
            for (i in 0...4) {
                var nr = r + dr[i]
                var nc = c + dc[i]
                if (nr >= 0 && nr < h && nc >= 0 && nc < w && grid[nr][nc] != 35) {
                    var pr = r
                    var pc = c
                    var cr = nr
                    var cc = nc
                    var dist = 1
                    while (!isNode[cr][cc]) {
                        var nextR = -1
                        var nextC = -1
                        for (j in 0...4) {
                            var tr = cr + dr[j]
                            var tc = cc + dc[j]
                            if (tr >= 0 && tr < h && tc >= 0 && tc < w && grid[tr][tc] != 35 && (tr != pr || tc != pc)) {
                                nextR = tr
                                nextC = tc
                                break
                            }
                        }
                        pr = cr
                        pc = cc
                        cr = nextR
                        cc = nextC
                        dist = dist + 1
                    }
                    var key = "%(cr),%(cc)"
                    var vid = nodeToId[key]
                    adjV[uid].add(vid)
                    adjW[uid].add(dist)
                }
            }
        }
        
        var startId = nodeToId["%(sy),%(sx)"]
        var targetId = nodeToId["%(ey),%(ex)"]
        
        var vis = List.filled(nodeId, false)
        
        var dfs
        dfs = Fn.new { |u, d|
            if (u == targetId) return d
            vis[u] = true
            var max = -1
            var neighbors = adjV[u]
            var weights = adjW[u]
            var len = neighbors.count
            for (i in 0...len) {
                var v = neighbors[i]
                if (!vis[v]) {
                    var res = dfs.call(v, d + weights[i])
                    if (res > max) max = res
                }
            }
            vis[u] = false
            return max
        }
        
        System.print(dfs.call(startId, 0))
    }
}

Day23.run()
