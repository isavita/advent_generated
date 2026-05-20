
import "io" for File

var content = File.read("input.txt")
var rules = []
var sum = 0
for (line in content.split("\n")) {
  var l = line.trim()
  if (l != "") {
    if (l.contains("|")) {
      rules.add(l.split("|").map { |x| Num.fromString(x) }.toList)
    } else {
      var upd = l.split(",").map { |x| Num.fromString(x) }.toList
      var pos = {}
      for (i in 0...upd.count) pos[upd[i]] = i
      var ok = true
      for (r in rules) {
        if (pos.containsKey(r[0]) && pos.containsKey(r[1]) && pos[r[0]] > pos[r[1]]) {
          ok = false
          break
        }
      }
      if (ok) sum = sum + upd[(upd.count / 2).floor]
    }
  }
}
System.print(sum)
