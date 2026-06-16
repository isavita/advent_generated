
import "io" for File

var TOTAL_ROWS = 40
var current = File.read("input.txt").trim()
var len = current.count
var safeCount = 0

for (i in 0...len) {
  if (current[i] == ".") safeCount = safeCount + 1
}

for (row in 1...TOTAL_ROWS) {
  var next = List.filled(len, ".")

  for (i in 0...len) {
    var left = i == 0 ? "." : current[i - 1]
    var right = i == len - 1 ? "." : current[i + 1]

    if (left == right) {
      safeCount = safeCount + 1
    } else {
      next[i] = "^"
    }
  }

  current = next.join()
}

System.print(safeCount)
