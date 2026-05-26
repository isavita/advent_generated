
import "io" for File

var content = File.read("input.txt")
var digits = "0123456789-"
var numbers = []
var cur = ""
for (c in content) {
  if (digits.contains(c)) {
    cur = cur + c
  } else {
    if (cur != "" && cur != "-") {
      numbers.add(Num.fromString(cur))
    }
    cur = ""
  }
}
if (cur != "" && cur != "-") {
  numbers.add(Num.fromString(cur))
}

var off = 10000000000000
var s = 0
var t = 0

var i = 0
while (i + 5 < numbers.count) {
  var ax = numbers[i]
  var ay = numbers[i+1]
  var bx = numbers[i+2]
  var by = numbers[i+3]
  var px = numbers[i+4] + off
  var py = numbers[i+5] + off

  var d = ax * by - ay * bx
  if (d != 0) {
    var na = px * by - py * bx
    var nb = py * ax - px * ay
    var a = (na / d).round
    var b = (nb / d).round
    if (a * d == na && b * d == nb && a >= 0 && b >= 0) {
      s = s + 1
      t = t + 3 * a + b
    }
  }
  i = i + 6
}

System.print("%(s) %(t)")
