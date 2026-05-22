
import "io" for File

var content = File.read("input.txt").split("\r").join("")
var lines = content.split("\n")

var seq = lines[0].split(",").map { |x| Num.fromString(x) }.toList

var boards = []
var currentBoard = []
for (i in 1...lines.count) {
  var line = lines[i]
  var row = line.split(" ").where { |x| x != "" }.map { |x| Num.fromString(x) }.toList
  if (row.isEmpty) {
    if (currentBoard.count > 0) {
      boards.add(currentBoard)
      currentBoard = []
    }
  } else {
    currentBoard.add(row)
  }
}
if (currentBoard.count > 0) boards.add(currentBoard)

var solve = Fn.new {
  for (num in seq) {
    for (b in boards) {
      for (r in 0...5) {
        for (c in 0...5) {
          if (b[r][c] == num) b[r][c] = null
        }
      }

      var win = false
      for (r in 0...5) {
        var all = true
        for (c in 0...5) {
          if (b[r][c] != null) {
            all = false
            break
          }
        }
        if (all) {
          win = true
          break
        }
      }
      if (!win) {
        for (c in 0...5) {
          var all = true
          for (r in 0...5) {
            if (b[r][c] != null) {
              all = false
              break
            }
          }
          if (all) {
            win = true
            break
          }
        }
      }

      if (win) {
        var sum = 0
        for (r in 0...5) {
          for (c in 0...5) {
            if (b[r][c] != null) sum = sum + b[r][c]
          }
        }
        System.print(sum * num)
        return
      }
    }
  }
}

solve.call()
