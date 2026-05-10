
import "io" for File

class Disc {
  construct new(positions, start) {
    _positions = positions
    _start = start
  }
  positions { _positions }
  start { _start }
}

var discs = []
var text = File.read("input.txt")
for (line in text.split("\n")) {
  if (line.count == 0) continue
  var parts = line.split(" ")
  var positions = Num.fromString(parts[3]).floor
  var s = parts[11]
  // strip trailing punctuation (e.g. the final '.')
  if (s.endsWith(".") || s.endsWith(",")) s = s[0...-1]
  var start = Num.fromString(s).floor
  discs.add(Disc.new(positions, start))
}

var time = 0
while (true) {
  var ok = true
  for (i in 0...discs.count) {
    var d = discs[i]
    if ((d.start + time + i + 1) % d.positions != 0) {
      ok = false
      break
    }
  }
  if (ok) {
    System.print(time)
    break
  }
  time = time + 1
}
