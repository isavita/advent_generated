
import "io" for File

var input = File.read("input.txt")
var len = input.count
var pos = 0
var total = 0
var enabled = true

while (pos < len) {
  if (pos + 4 <= len && input[pos...pos+4] == "mul(") {
    var p = pos + 4
    var n1 = 0
    var l1 = 0
    while (p < len && "0123456789".contains(input[p])) {
      n1 = n1 * 10 + (input[p].bytes[0] - 48)
      p = p + 1
      l1 = l1 + 1
    }
    if (l1 > 0 && p < len && input[p] == ",") {
      p = p + 1
      var n2 = 0
      var l2 = 0
      while (p < len && "0123456789".contains(input[p])) {
        n2 = n2 * 10 + (input[p].bytes[0] - 48)
        p = p + 1
        l2 = l2 + 1
      }
      if (l2 > 0 && p < len && input[p] == ")") {
        if (enabled) total = total + n1 * n2
        pos = p + 1
        continue
      }
    }
    pos = pos + 1
  } else if (pos + 4 <= len && input[pos...pos+4] == "do()") {
    enabled = true
    pos = pos + 4
  } else if (pos + 7 <= len && input[pos...pos+7] == "don't()") {
    enabled = false
    pos = pos + 7
  } else {
    pos = pos + 1
  }
}

System.print(total)
