import "io" for File

class Main {
  static main() {
    var content = File.read("input.txt")
    var lines = []
    for (line in content.split("\n")) {
      if (line != "") lines.add(line)
    }
    if (lines.isEmpty) {
      System.print("Total rolls removed: 0")
      return
    }
    var grid = []
    for (line in lines) {
      var row = []
      for (ch in line) row.add(ch)
      grid.add(row)
    }
    var rows = grid.count
    var cols = grid[0].count
    var total = 0
    while (true) {
      var toRemove = []
      for (r in 0...rows) {
        for (c in 0...cols) {
          if (grid[r][c] == "@") {
            var cnt = 0
            for (dr in [-1, 0, 1]) {
              for (dc in [-1, 0, 1]) {
                if (dr != 0 || dc != 0) {
                  var nr = r + dr
                  var nc = c + dc
                  if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] == "@") {
                    cnt = cnt + 1
                  }
                }
              }
            }
            if (cnt < 4) toRemove.add([r, c])
          }
        }
      }
      if (toRemove.isEmpty) {
        System.print("Total rolls removed: %(total)")
        return
      }
      total = total + toRemove.count
      for (pos in toRemove) {
        grid[pos[0]][pos[1]] = "."
      }
    }
  }
}

Main.main()