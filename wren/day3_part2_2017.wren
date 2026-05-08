
import "io" for File

class Spiral {
  static solve(input) {
    var grid = {}
    var x = 0
    var y = 0
    grid["0,0"] = 1
    if (1 > input) return 1
    var dir = 0
    var step = 1
    var steps = 0
    var changeDir = false
    while (true) {
      if (dir == 0) x = x + 1
      if (dir == 1) y = y - 1
      if (dir == 2) x = x - 1
      if (dir == 3) y = y + 1
      var sum = 0
      for (dx in -1..1) {
        for (dy in -1..1) {
          if (dx == 0 && dy == 0) continue
          var key = "%(x+dx),%(y+dy)"
          var val = grid[key]
          if (val) sum = sum + val
        }
      }
      grid["%(x),%(y)"] = sum
      if (sum > input) return sum
      steps = steps + 1
      if (steps == step) {
        steps = 0
        if (changeDir) step = step + 1
        changeDir = !changeDir
        dir = (dir + 1) % 4
      }
    }
  }
}

var input = Num.fromString(File.read("input.txt").trim())
System.print(Spiral.solve(input))
