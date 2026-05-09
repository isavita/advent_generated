import "io" for File

var abs = Fn.new { |n| n < 0 ? -n : n }

var main = Fn.new {
  var content = File.read("input.txt").trim()
  var lines = content.split("\n")
  var height = lines.count
  var width = lines[0].count

  var galaxies = []
  var isEmptyRow = List.filled(height, true)
  var isEmptyCol = List.filled(width, true)

  for (y in 0...height) {
    for (x in 0...width) {
      var ch = lines[y][x]
      if (ch != ".") {
        galaxies.add([x, y])
        isEmptyRow[y] = false
        isEmptyCol[x] = false
      }
    }
  }

  var emptyRowPrefix = List.filled(height, 0)
  var count = 0
  for (y in 0...height) {
    emptyRowPrefix[y] = count
    if (isEmptyRow[y]) count = count + 1
  }

  var emptyColPrefix = List.filled(width, 0)
  count = 0
  for (x in 0...width) {
    emptyColPrefix[x] = count
    if (isEmptyCol[x]) count = count + 1
  }

  var expansionFactor = 1000000
  var expansionAdd = expansionFactor - 1

  var expandedGalaxies = []
  for (galaxy in galaxies) {
    var x = galaxy[0]
    var y = galaxy[1]
    var newX = x + emptyColPrefix[x] * expansionAdd
    var newY = y + emptyRowPrefix[y] * expansionAdd
    expandedGalaxies.add([newX, newY])
  }

  var total = 0
  var n = expandedGalaxies.count
  for (i in 0...n) {
    for (j in i+1...n) {
      var g1 = expandedGalaxies[i]
      var g2 = expandedGalaxies[j]
      var dx = abs.call(g1[0] - g2[0])
      var dy = abs.call(g1[1] - g2[1])
      total = total + dx + dy
    }
  }

  System.print(total)
}

main.call()