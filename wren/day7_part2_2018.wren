
import "io" for File

var alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var lines = File.read("input.txt").split("\n")
var adj = {}
var deg = {}
var nodes = {}

for (line in lines) {
  if (line.count > 0) {
    var parts = line.split(" ")
    if (parts.count >= 8) {
      var u = parts[1]
      var v = parts[7]
      if (!adj.containsKey(u)) adj[u] = []
      adj[u].add(v)
      deg[v] = (deg[v] || 0) + 1
      nodes[u] = true
      nodes[v] = true
    }
  }
}

for (n in nodes.keys) {
  if (!deg.containsKey(n)) deg[n] = 0
  if (!adj.containsKey(n)) adj[n] = []
}

var dur = {}
for (n in nodes.keys) {
  dur[n] = alphabet.indexOf(n) + 61
}

var job = ["", "", "", "", ""]
var rem = [0, 0, 0, 0, 0]
var start = {}
var done = 0
var time = 0

while (done < nodes.keys.count) {
  for (i in 0...5) {
    if (rem[i] > 0) {
      rem[i] = rem[i] - 1
      if (rem[i] == 0) {
        var t = job[i]
        job[i] = ""
        done = done + 1
        for (v in adj[t]) {
          deg[v] = deg[v] - 1
        }
      }
    }
  }
  for (i in 0...5) {
    if (job[i] == "") {
      for (k in 0...26) {
        var n = alphabet[k]
        if (nodes.containsKey(n) && deg[n] <= 0 && !start.containsKey(n)) {
          job[i] = n
          rem[i] = dur[n]
          start[n] = true
          break
        }
      }
    }
  }
  if (done == nodes.keys.count) break
  time = time + 1
}

System.print(time)
