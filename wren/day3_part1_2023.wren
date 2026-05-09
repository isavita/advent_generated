
import "io" for File

var isDigit = Fn.new { |c| c.bytes[0] >= 48 && c.bytes[0] <= 57 }

var extractNumber = Fn.new { |matrix, x, y|
  var line = matrix[y]
  var numStr = ""
  var currentX = x
  while (currentX < line.count && isDigit.call(line[currentX])) {
    numStr = numStr + line[currentX]
    currentX = currentX + 1
  }
  return [Num.fromString(numStr), currentX - x]
}

var checkAdjacent = Fn.new { |matrix, x, y|
  for (dy in -1..1) {
    for (dx in -1..1) {
      var adjX = x + dx
      var adjY = y + dy
      if (adjY >= 0 && adjY < matrix.count) {
        var line = matrix[adjY]
        if (adjX >= 0 && adjX < line.count) {
          var cell = line[adjX]
          if (!isDigit.call(cell) && cell != ".") {
            return true
          }
        }
      }
    }
  }
  return false
}

var isAdjacentToSymbol = Fn.new { |matrix, x, y, length|
  for (i in 0...length) {
    if (checkAdjacent.call(matrix, x + i, y)) {
      return true
    }
  }
  return false
}

var sumPartNumbers = Fn.new { |matrix|
  var total = 0
  for (y in 0...matrix.count) {
    var line = matrix[y]
    var x = 0
    while (x < line.count) {
      if (isDigit.call(line[x])) {
        var tuple = extractNumber.call(matrix, x, y)
        var number = tuple[0]
        var len = tuple[1]
        if (isAdjacentToSymbol.call(matrix, x, y, len)) {
          total = total + number
        }
        x = x + len
      } else {
        x = x + 1
      }
    }
  }
  return total
}

var content = File.read("input.txt")
var lines = content.split("\n")
System.print(sumPartNumbers.call(lines))
