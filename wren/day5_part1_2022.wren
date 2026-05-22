
import "io" for File

var s = {}
var max = 0
for (line in File.read("input.txt").replace("\r", "").split("\n")) {
  if (line.contains("[")) {
    var n = ((line.count + 1) / 4).floor
    for (c in 0...n) {
      var idx = 1 + 4 * c
      if (idx < line.count) {
        var char = line[idx]
        if (char != " ") {
          if (!s[c]) s[c] = []
          s[c].insert(0, char)
          if (c > max) max = c
        }
      }
    }
  } else {
    var p = line.split(" ")
    if (p.count > 0 && p[0] == "move") {
      var num = Num.fromString(p[1])
      var from = Num.fromString(p[3]) - 1
      var to = Num.fromString(p[5]) - 1
      for (i in 0...num) {
        if (!s[to]) s[to] = []
        s[to].add(s[from].removeAt(-1))
      }
    }
  }
}

var ans = ""
for (i in 0..max) {
  if (s[i] && s[i].count > 0) ans = ans + s[i][-1]
}
System.print(ans)
