import "io" for File

class Main {
  static getPointsWithSteps(path) {
    var points = {}
    var x = 0
    var y = 0
    var steps = 0
    var moves = path.split(",")
    for (move in moves) {
      var dir = move[0]
      var dist = Num.fromString(move[1..-1]).truncate
      for (j in 0...dist) {
        steps = steps + 1
        if (dir == "U") {
          y = y + 1
        } else if (dir == "D") {
          y = y - 1
        } else if (dir == "L") {
          x = x - 1
        } else if (dir == "R") {
          x = x + 1
        }
        var key = "%(x),%(y)"
        if (!points.containsKey(key)) {
          points[key] = steps
        }
      }
    }
    return points
  }

  static main() {
    var content = File.read("input.txt")
    var lines = content.split("\n")
    var nonEmpty = []
    for (line in lines) {
      if (line.trim() != "") {
        nonEmpty.add(line)
      }
    }
    var wire1Path = nonEmpty[0]
    var wire2Path = nonEmpty[1]
    var wire1 = getPointsWithSteps(wire1Path)
    var wire2 = getPointsWithSteps(wire2Path)
    var minSteps = 1000000000
    for (key in wire1.keys) {
      if (wire2.containsKey(key)) {
        var sum = wire1[key] + wire2[key]
        if (sum < minSteps) {
          minSteps = sum
        }
      }
    }
    System.print(minSteps)
  }
}
Main.main()