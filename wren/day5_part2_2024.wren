
import "io" for File

class Main {
  static parseInput(input) {
    var rules = {}  // map u -> map v -> true
    var updates = []
    var readingRules = true
    var lines = input.trim().split("\n")
    for (line in lines) {
      if (line.isEmpty) {
        readingRules = false
        continue
      }
      if (readingRules) {
        var parts = line.split("|")
        if (parts.count == 2) {
          var u = Num.fromString(parts[0].trim())
          var v = Num.fromString(parts[1].trim())
          if (u && v) {
            if (!rules[u]) rules[u] = {}
            rules[u][v] = true
          }
        }
      } else {
        var parts = line.split(",")
        var update = []
        for (p in parts) {
          var num = Num.fromString(p.trim())
          if (num) update.add(num)
        }
        updates.add(update)
      }
    }
    return [rules, updates]
  }

  static isOrderCorrect(update, rules, pagesInUpdate) {
    if (update.count <= 1) return true
    for (i in 0...update.count) {
      for (j in (i+1)...update.count) {
        var pageI = update[i]
        var pageJ = update[j]
        if (rules[pageJ] && rules[pageJ][pageI]) {
          return false
        }
      }
    }
    return true
  }

  static topologicalSort(update, rules, pagesInUpdate) {
    if (update.count <= 1) return update
    var adj = {}
    var inDegree = {}
    for (page in pagesInUpdate.keys) {
      inDegree[page] = 0
    }
    for (u in rules.keys) {
      if (!pagesInUpdate[u]) continue
      for (v in rules[u].keys) {
        if (!pagesInUpdate[v]) continue
        if (!adj[u]) adj[u] = []
        adj[u].add(v)
        inDegree[v] = inDegree[v] + 1
      }
    }
    var queue = []
    var initial = []
    for (page in pagesInUpdate.keys) {
      if (inDegree[page] == 0) initial.add(page)
    }
    initial.sort()
    for (node in initial) queue.add(node)
    var sorted = []
    while (queue.count > 0) {
      var u = queue.removeAt(0)
      sorted.add(u)
      if (adj[u]) {
        var neighbors = adj[u]
        neighbors.sort()
        for (v in neighbors) {
          inDegree[v] = inDegree[v] - 1
          if (inDegree[v] == 0) queue.add(v)
        }
      }
    }
    if (sorted.count != pagesInUpdate.count) {
      Fiber.abort("Cycle detected or error in sort")
    }
    return sorted
  }

  static getMiddlePage(update) {
    if (update.count == 0) Fiber.abort("Empty update")
    var mid = (update.count / 2).floor
    return update[mid]
  }

  static solve(input) {
    var parsed = parseInput(input)
    var rules = parsed[0]
    var updates = parsed[1]
    var part1 = 0
    var part2 = 0
    for (originalUpdate in updates) {
      if (originalUpdate.count == 0) continue
      var pagesInUpdate = {}
      for (page in originalUpdate) {
        pagesInUpdate[page] = true
      }
      if (isOrderCorrect(originalUpdate, rules, pagesInUpdate)) {
        part1 = part1 + getMiddlePage(originalUpdate)
      } else {
        var sortedUpdate = topologicalSort(originalUpdate, rules, pagesInUpdate)
        part2 = part2 + getMiddlePage(sortedUpdate)
      }
    }
    return [part1, part2]
  }

  static main() {
    var input = File.read("input.txt")
    var results = solve(input)
    System.print("Part 1: %(results[0])")
    System.print("Part 2: %(results[1])")
  }
}

Main.main()
