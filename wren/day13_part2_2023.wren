
import "io" for File

var diff = Fn.new { |s1, s2|
  var d = 0
  for (i in 0...s1.count) {
    if (s1[i] != s2[i]) d = d + 1
  }
  return d
}

var content = File.read("input.txt").replace("\r\n", "\n")
var blocks = content.split("\n\n")
var total = 0

for (block in blocks) {
  var R = block.split("\n").where { |line| line != "" }.toList
  if (R.isEmpty) continue
  var r = R.count
  var m = R[0].count
  var C = List.filled(m, "")
  for (col in 0...m) {
    var colStr = ""
    for (row in 0...r) {
      colStr = colStr + R[row][col]
    }
    C[col] = colStr
  }

  for (i in 0...(r - 1)) {
    var d = 0
    var j = 0
    while (i - j >= 0 && i + j + 1 < r) {
      d = d + diff.call(R[i - j], R[i + 1 + j])
      j = j + 1
    }
    if (d == 1) total = total + 100 * (i + 1)
  }

  for (i in 0...(m - 1)) {
    var d = 0
    var j = 0
    while (i - j >= 0 && i + j + 1 < m) {
      d = d + diff.call(C[i - j], C[i + 1 + j])
      j = j + 1
    }
    if (d == 1) total = total + (i + 1)
  }
}

System.print(total)
