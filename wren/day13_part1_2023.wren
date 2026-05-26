import "io" for File

var findReflection = Fn.new { |arr|
  var n = arr.count
  for (i in 0...(n - 1)) {
    var l = (i + 1 < n - 1 - i) ? i + 1 : n - 1 - i
    var match = true
    for (j in 0...l) {
      if (arr[i - j] != arr[i + 1 + j]) {
        match = false
        break
      }
    }
    if (match) return i + 1
  }
  return 0
}

var content = File.read("input.txt").replace("\r\n", "\n")
var grids = content.split("\n\n")
var total = 0
for (grid in grids) {
  var rows = grid.split("\n").where { |line| line != "" }.toList
  if (rows.isEmpty) continue
  var R = rows.count
  var C = rows[0].count
  var cols = List.filled(C, "")
  for (r in 0...R) {
    for (c in 0...C) {
      cols[c] = cols[c] + rows[r][c]
    }
  }
  total = total + findReflection.call(cols) + 100 * findReflection.call(rows)
}
System.print(total)