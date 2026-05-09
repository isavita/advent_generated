import "io" for File

class Solver {
  static countArrangements(springs, groups, springIdx, groupIdx, memo) {
    var key = "%(springIdx),%(groupIdx)"
    if (memo.containsKey(key)) return memo[key]

    if (groupIdx == groups.count) {
      var i = springIdx
      while (i < springs.count) {
        if (springs[i] == "#") {
          memo[key] = 0
          return 0
        }
        i = i + 1
      }
      memo[key] = 1
      return 1
    }

    if (springIdx >= springs.count) {
      memo[key] = 0
      return 0
    }

    var result = 0
    var currentSpring = springs[springIdx]
    var currentGroupLen = groups[groupIdx]

    if (currentSpring == "." || currentSpring == "?") {
      result = result + countArrangements(springs, groups, springIdx + 1, groupIdx, memo)
    }

    if (currentSpring == "#" || currentSpring == "?") {
      var canPlace = true
      if (springIdx + currentGroupLen > springs.count) {
        canPlace = false
      } else {
        var i = springIdx
        while (i < springIdx + currentGroupLen) {
          if (springs[i] == ".") {
            canPlace = false
            break
          }
          i = i + 1
        }
        if (canPlace && springIdx + currentGroupLen < springs.count) {
          if (springs[springIdx + currentGroupLen] == "#") {
            canPlace = false
          }
        }
      }
      if (canPlace) {
        var nextSpringIdx = springIdx + currentGroupLen + 1
        result = result + countArrangements(springs, groups, nextSpringIdx, groupIdx + 1, memo)
      }
    }

    memo[key] = result
    return result
  }

  static solve(lines, unfoldFactor) {
    var total = 0
    for (line in lines) {
      if (line.trim() == "") continue
      var parts = line.split(" ")
      if (parts.count != 2) {
        System.print("Warning: invalid line: " + line)
        continue
      }
      var springs = parts[0]
      var groupsStr = parts[1]
      var groups = groupsStr.split(",").map {|s| Num.fromString(s) }.toList

      if (unfoldFactor > 1) {
        var unfoldedSprings = ""
        var unfoldedGroups = []
        for (i in 0...unfoldFactor) {
          if (i > 0) unfoldedSprings = unfoldedSprings + "?"
          unfoldedSprings = unfoldedSprings + springs
          unfoldedGroups.addAll(groups)
        }
        springs = unfoldedSprings
        groups = unfoldedGroups
      }

      var memo = {}
      var arrangements = countArrangements(springs, groups, 0, 0, memo)
      total = total + arrangements
    }
    return total
  }
}

var content = File.read("input.txt")
var rawLines = content.split("\n")
var lines = []
for (rawLine in rawLines) {
  var line = rawLine.trim()
  if (line != "") lines.add(line)
}
var part1 = Solver.solve(lines, 1)
System.print("Part 1: %(part1)")
var part2 = Solver.solve(lines, 5)
System.print("Part 2: %(part2)")