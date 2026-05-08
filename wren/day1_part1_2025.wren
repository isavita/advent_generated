import "io" for File

var data = File.read("input.txt")
var lines = data.split("\n")
var TOTAL = 100
var pos = 50
var count = 0
for (line in lines) {
  var trimmed = line.trim()
  if (trimmed.count == 0) continue
  var dir = trimmed[0]
  var dist = Num.fromString(trimmed[1..-1])
  var delta = dir == "L" ? -dist : dist
  pos = (pos + delta) % TOTAL
  if (pos < 0) pos = pos + TOTAL
  if (pos == 0) count = count + 1
}
System.print(count)