
import "io" for File

var input = File.read("input.txt").split("\n")
var rows = input.count
var cols = input[0].count

var grid = List.filled(rows, null)
for (i in 0...rows) {
  grid[i] = input[i].toList
}

var tiltNorth = Fn.new {
  for (col in 0...cols) {
    var nextEmpty = 0
    for (row in 0...rows) {
      if (grid[row][col] == "O") {
        if (row != nextEmpty) {
          grid[nextEmpty][col] = "O"
          grid[row][col] = "."
        }
        nextEmpty = nextEmpty + 1
      } else if (grid[row][col] == "#") {
        nextEmpty = row + 1
      }
    }
  }
}

var calculateLoad = Fn.new {
  var totalLoad = 0
  for (row in 0...rows) {
    for (col in 0...cols) {
      if (grid[row][col] == "O") {
        totalLoad = totalLoad + (rows - row)
      }
    }
  }
  return totalLoad
}

tiltNorth.call()
var load = calculateLoad.call()
System.print(load)
