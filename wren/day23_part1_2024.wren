
import "io" for File

var content = File.read("input.txt")
var lines = content.replace("\r", "").split("\n")

var edges = []
var nodes_map = {}
var nodes_list = []

for (line in lines) {
    if (line == "") continue
    var parts = line.split("-")
    if (parts.count != 2) continue
    var u = parts[0]
    var v = parts[1]
    if (!nodes_map.containsKey(u)) {
        nodes_map[u] = nodes_list.count
        nodes_list.add(u)
    }
    if (!nodes_map.containsKey(v)) {
        nodes_map[v] = nodes_list.count
        nodes_list.add(v)
    }
    edges.add([nodes_map[u], nodes_map[v]])
}

var n = nodes_list.count
var adj = List.filled(n, null)
for (i in 0...n) {
    adj[i] = List.filled(n, false)
}

for (edge in edges) {
    var u = edge[0]
    var v = edge[1]
    adj[u][v] = true
    adj[v][u] = true
}

var count = 0
for (i in 0...n) {
    for (j in (i + 1)...n) {
        if (adj[i][j]) {
            for (k in (j + 1)...n) {
                if (adj[j][k] && adj[k][i]) {
                    if (nodes_list[i].startsWith("t") ||
                        nodes_list[j].startsWith("t") ||
                        nodes_list[k].startsWith("t")) {
                        count = count + 1
                    }
                }
            }
        }
    }
}

System.print(count)
