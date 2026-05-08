
import "io" for File

var checkMAS = Fn.new { |grid, x, y, dx, dy|
  var word = "MAS"
  var rows = grid.count
  var cols = grid[0].count

  // Check forward (MAS)
  var forward = true
  for (i in 0...word.count) {
    var nx = x + dx * i
    var ny = y + dy * i
    if (nx < 0 || ny < 0 || nx >= rows || ny >= cols) {
      forward = false
      break
    }
    if (grid[nx][ny] != word[i]) {
      forward = false
      break
    }
  }
  if (forward) return true

  // Check backward (SAM)
  var backward = true
  for (i in 0...word.count) {
    var nx = x + dx * i
    var ny = y + dy * i
    if (nx < 0 || ny < 0 || nx >= rows || ny >= cols) {
      backward = false
      break
    }
    if (grid[nx][ny] != word[word.count - 1 - i]) {
      backward = false
      break
    }
  }
  return backward
}

var checkXMAS = Fn.new { |grid, x, y|
  // Condition 1: diagonals starting from top-left and top-right
  if (checkMAS.call(grid, x-1, y-1, 1, 1) && checkMAS.call(grid, x-1, y+1, 1, -1)) {
    return true
  }
  // Condition 2: diagonals starting from bottom-left and bottom-right
  if (checkMAS.call(grid, x+1, y-1, -1, 1) && checkMAS.call(grid, x+1, y+1, -1, -1)) {
    return true
  }
  return false
}

var countXMASPatterns = Fn.new { |grid|
  if (grid.count < 3) return 0
  var cols = grid[0].count
  if (cols < 3) return 0
  var count = 0
  for (i in 1...grid.count-1) {
    for (j in 1...grid[i].count-1) {
      if (grid[i][j] == "A") {
        if (checkXMAS.call(grid, i, j)) {
          count = count + 1
        }
      }
    }
  }
  return count
}

var lines = File.read("input.txt").split("\n").where { |line| line.trim().count > 0 }.toList
var count = countXMASPatterns.call(lines)
System.print("X-MAS patterns appear %(count) times in the word search")
