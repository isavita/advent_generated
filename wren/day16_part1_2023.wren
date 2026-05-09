
import "io" for File

class Main {
  static solve(grid) {
    if (grid.count == 0 || grid[0].count == 0) return 0
    var rows = grid.count
    var cols = grid[0].count

    var queue = []
    var head = 0
    var visitedStates = {}
    var energizedTiles = {}

    queue.add([0, 0, 0, 1]) // r, c, dr, dc

    while (head < queue.count) {
      var state = queue[head]
      head = head + 1
      var r = state[0]
      var c = state[1]
      var dr = state[2]
      var dc = state[3]

      if (r < 0 || r >= rows || c < 0 || c >= cols) continue

      var stateKey = "%(r),%(c),%(dr),%(dc)"
      if (visitedStates[stateKey] == true) continue
      visitedStates[stateKey] = true

      var posKey = "%(r),%(c)"
      energizedTiles[posKey] = true

      var tile = grid[r][c]
      var nextDirs = []

      if (tile == ".") {
        nextDirs.add([dr, dc])
      } else if (tile == "/") {
        nextDirs.add([-dc, -dr])
      } else if (tile == "\\") {
        nextDirs.add([dc, dr])
      } else if (tile == "|") {
        if (dr == 0) {
          nextDirs.add([-1, 0])
          nextDirs.add([1, 0])
        } else {
          nextDirs.add([dr, dc])
        }
      } else if (tile == "-") {
        if (dc == 0) {
          nextDirs.add([0, -1])
          nextDirs.add([0, 1])
        } else {
          nextDirs.add([dr, dc])
        }
      }

      for (nextDir in nextDirs) {
        queue.add([r + nextDir[0], c + nextDir[1], nextDir[0], nextDir[1]])
      }
    }

    var count = 0
    for (key in energizedTiles) count = count + 1
    return count
  }

  static main() {
    var filename = "input.txt"
    var content = File.read(filename)
    var trimmed = content.trim()
    var grid = trimmed.split("\n")
    var filtered = []
    for (line in grid) {
      if (line != "") filtered.add(line)
    }
    System.print(solve(filtered))
  }
}

Main.main()
