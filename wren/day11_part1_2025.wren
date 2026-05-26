
import "io" for File

class Solver {
  static run() {
    __adj = {}
    __memo = {}
    var content = File.read("input.txt")
    for (line in content.split("\n")) {
      var trimmed = line.trim()
      if (trimmed == "") continue
      var parts = trimmed.split(":")
      var node = parts[0].trim()
      var neighs = []
      if (parts.count > 1) {
        for (v in parts[1].split(" ")) {
          var tv = v.trim()
          if (tv != "") neighs.add(tv)
        }
      }
      __adj[node] = neighs
    }
    System.print(dfs("you"))
  }

  static dfs(u) {
    if (u == "out") return 1
    if (__memo.containsKey(u)) return __memo[u]
    var cnt = 0
    if (__adj.containsKey(u)) {
      for (v in __adj[u]) {
        cnt = cnt + dfs(v)
      }
    }
    __memo[u] = cnt
    return cnt
  }
}

Solver.run()
