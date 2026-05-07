
import "io" for File

class Circuit {
  static solve(wire, graph, memo) {
    if (memo.containsKey(wire)) return memo[wire]
    
    var val = Num.fromString(wire)
    if (val != null) return val

    var rule = graph[wire]
    var p = rule.split(" ")
    var res = 0

    if (p.count == 1) {
      res = solve(p[0], graph, memo)
    } else if (p[0] == "NOT") {
      res = ~solve(p[1], graph, memo)
    } else if (p[1] == "AND") {
      res = solve(p[0], graph, memo) & solve(p[2], graph, memo)
    } else if (p[1] == "OR") {
      res = solve(p[0], graph, memo) | solve(p[2], graph, memo)
    } else if (p[1] == "LSHIFT") {
      res = solve(p[0], graph, memo) << solve(p[2], graph, memo)
    } else if (p[1] == "RSHIFT") {
      res = solve(p[0], graph, memo) >> solve(p[2], graph, memo)
    }

    res = res & 0xFFFF
    memo[wire] = res
    return res
  }
}

var graph = {}
var lines = File.read("input.txt").trim().split("\n")
for (line in lines) {
  var parts = line.split(" -> ")
  graph[parts[1]] = parts[0]
}

var a1 = Circuit.solve("a", graph, {})
graph["b"] = a1.toString
System.print(Circuit.solve("a", graph, {}))
