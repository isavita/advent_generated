
import "io" for File

var input = File.read("input.txt")
var lines = input.split("\n")
while (lines.count > 0 && lines[-1] == "") lines.removeLast()
if (lines.count == 0) {
  System.print(0)
  return
}

var width = lines[0].count
var height = lines.count

var data = []
for (y in 0...height) {
  var line = lines[y]
  for (x in 0...width) {
    if (line[x] != ".") data.add([x, y])
  }
}

var rowHas = List.filled(height, false)
for (coord in data) rowHas[coord[1]] = true
var emptyRows = []
for (y in 0...height) if (!rowHas[y]) emptyRows.add(y)

var colHas = List.filled(width, false)
for (coord in data) colHas[coord[0]] = true
var emptyCols = []
for (x in 0...width) if (!colHas[x]) emptyCols.add(x)

var calculateOffsets = Fn.new { |emptyIndexes, bound|
  var offsets = List.filled(bound, 0)
  for (idx in emptyIndexes) {
    for (i in (idx+1)...bound) offsets[i] = offsets[i] + 1
  }
  return offsets
}

var expansionFactor = 2
var numLinesToAdd = expansionFactor - 1
var dXs = calculateOffsets.call(emptyCols, width)
var dYs = calculateOffsets.call(emptyRows, height)

var newData = []
for (coord in data) {
  var x = coord[0]
  var y = coord[1]
  newData.add([x + dXs[x] * numLinesToAdd, y + dYs[y] * numLinesToAdd])
}

var sum = 0
var n = newData.count
for (i in 0...n) {
  for (j in 0...i) {
    var c1 = newData[i]
    var c2 = newData[j]
    sum = sum + (c1[0] - c2[0]).abs + (c1[1] - c2[1]).abs
  }
}
System.print(sum)
