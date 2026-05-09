
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
var sum = 0
for (line in lines) {
  var first = -1
  var last = -1
  for (ch in line) {
    var b = ch.bytes[0]
    if (b >= 48 && b <= 57) {
      var d = b - 48
      if (first == -1) first = d
      last = d
    }
  }
  if (first != -1 && last != -1) sum = sum + first * 10 + last
}
System.print(sum)
