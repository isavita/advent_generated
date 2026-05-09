
import "io" for File

class Solver {
  static countArrangements(springs, groups, springIdx, groupIdx, currentGroupLen, cache) {
    var key = "%(springIdx),%(groupIdx),%(currentGroupLen)"
    if (cache.containsKey(key)) return cache[key]

    if (springIdx == springs.count) {
      if (groupIdx == groups.count && currentGroupLen == 0) return 1
      if (groupIdx == groups.count - 1 && currentGroupLen == groups[groupIdx]) return 1
      return 0
    }

    var total = 0
    var ch = springs[springIdx]

    if (ch == "." || ch == "?") {
      if (currentGroupLen == 0) {
        total = total + countArrangements(springs, groups, springIdx + 1, groupIdx, 0, cache)
      } else {
        if (groupIdx < groups.count && currentGroupLen == groups[groupIdx]) {
          total = total + countArrangements(springs, groups, springIdx + 1, groupIdx + 1, 0, cache)
        }
      }
    }

    if (ch == "#" || ch == "?") {
      if (groupIdx < groups.count && currentGroupLen < groups[groupIdx]) {
        total = total + countArrangements(springs, groups, springIdx + 1, groupIdx, currentGroupLen + 1, cache)
      }
    }

    cache[key] = total
    return total
  }

  static solve(filename) {
    var content = File.read(filename).trim()
    var lines = content.split("\n")
    var total = 0

    for (line in lines) {
      if (line == "") continue
      var parts = line.split(" ")
      if (parts.count != 2) continue

      var springsRaw = parts[0]
      var groupsStr = parts[1]
      var groups = groupsStr.split(",").map { |s| Num.fromString(s) }.toList
      var springs = springsRaw + "."

      var cache = {}
      total = total + countArrangements(springs, groups, 0, 0, 0, cache)
    }

    return total
  }
}

System.print(Solver.solve("input.txt"))
