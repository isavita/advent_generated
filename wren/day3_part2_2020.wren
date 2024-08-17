import "io" for File

var input = File.read("input.txt")
var lines = input.split("\n")

var slopes = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2]
]

var product = 1
for (slope in slopes) {
  var treeCount = 0
  var pos = 0
  var i = 0
  while (i < lines.count) {
    if (lines[i][pos] == "#") {
      treeCount = treeCount + 1
    }
    pos = (pos + slope[0]) % lines[i].count
    i = i + slope[1]
  }
  product = product * treeCount
}

System.print(product)