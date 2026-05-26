
import "io" for File

class Program {
  static run() {
    var content = File.read("input.txt")
    var lines = []
    for (line in content.split("\n")) {
      if (line.endsWith("\r")) {
        line = line[0...-1]
      }
      if (line.count > 0) {
        lines.add(line)
      }
    }

    if (lines.count == 0) {
      System.print(0)
      return
    }

    var H = lines.count
    var W = lines[0].count

    var grid = List.filled(W * H, "")
    for (y in 0...H) {
      for (x in 0...W) {
        grid[y * W + x] = lines[y][x]
      }
    }

    var shiftSingleRock = Fn.new { |x, y, dx, dy|
      if (grid[y * W + x] == "O") {
        var nx = x + dx
        var ny = y + dy
        while (nx >= 0 && nx < W && ny >= 0 && ny < H && grid[ny * W + nx] == ".") {
          grid[ny * W + nx] = "O"
          grid[y * W + x] = "."
          x = nx
          y = ny
          nx = x + dx
          ny = y + dy
        }
      }
    }

    var shiftRocks = Fn.new { |dx, dy|
      if (dy < 0 || dx < 0) {
        for (x in 0...W) {
          for (y in 0...H) {
            shiftSingleRock.call(x, y, dx, dy)
          }
        }
      } else {
        for (x in W-1..0) {
          for (y in H-1..0) {
            shiftSingleRock.call(x, y, dx, dy)
          }
        }
      }
    }

    var cycleRocks = Fn.new {
      shiftRocks.call(0, -1)
      shiftRocks.call(-1, 0)
      shiftRocks.call(0, 1)
      shiftRocks.call(1, 0)
    }

    var calculateLoad = Fn.new {
      var load = 0
      for (y in 0...H) {
        for (x in 0...W) {
          if (grid[y * W + x] == "O") {
            load = load + (H - y)
          }
        }
      }
      return load
    }

    var cache = {}
    var numCycles = 1000000000
    var i = 0
    while (i < numCycles) {
      var key = grid.join("")
      if (cache.containsKey(key)) {
        var prev = cache[key]
        var cycleLen = i - prev
        var remaining = (numCycles - prev) % cycleLen
        for (t in 0...remaining) {
          cycleRocks.call()
        }
        System.print(calculateLoad.call())
        return
      }
      cache[key] = i
      cycleRocks.call()
      i = i + 1
    }
    System.print(calculateLoad.call())
  }
}

Program.run()
