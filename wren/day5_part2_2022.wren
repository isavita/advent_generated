
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n").map { |l| l.endsWith("\r") ? l[0...-1] : l }.toList

var stacks = List.filled(100, "")
var phase2 = false
var maxStackIdx = 0

for (line in lines) {
  if (line.count == 0) continue
  if (!phase2) {
    if (line.contains(" 1 ")) {
      phase2 = true
      continue
    }
    var i = 1
    while (i < line.count) {
      var c = line[i]
      if ("ABCDEFGHIJKLMNOPQRSTUVWXYZ".contains(c)) {
        var idx = ((i - 1) / 4).floor
        if (idx > maxStackIdx) maxStackIdx = idx
        stacks[idx] = c + stacks[idx]
      }
      i = i + 4
    }
  } else {
    if (line.startsWith("move")) {
      var parts = line.split(" ")
      var n = Num.fromString(parts[1])
      var f = Num.fromString(parts[3]) - 1
      var t = Num.fromString(parts[5]) - 1
      var stackF = stacks[f]
      var len = stackF.count
      var mid = stackF[len - n...len]
      stacks[f] = stackF[0...len - n]
      stacks[t] = stacks[t] + mid
    }
  }
}

var ans = ""
for (i in 0..maxStackIdx) {
  var s = stacks[i]
  if (s.count > 0) {
    ans = ans + s[s.count - 1]
  }
}
System.print(ans)
