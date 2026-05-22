
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
if (lines.count > 0 && lines[-1] == "") {
  lines.removeAt(-1)
}

var total = 0
for (line in lines) {
  var s = line.trim()
  if (s.count == 0) continue
  var found = false
  for (d1 in 9..0) {
    var p = s.indexOf(d1.toString)
    if (p != -1 && p < s.count - 1) {
      var max_d2 = -1
      for (j in p+1...s.count) {
        var code = s[j].bytes[0]
        if (code >= 48 && code <= 57) {
          var digit = code - 48
          if (digit > max_d2) max_d2 = digit
        }
      }
      if (max_d2 != -1) {
        total = total + d1 * 10 + max_d2
        break
      }
    }
  }
}

System.print("Total output joltage: %(total)")
