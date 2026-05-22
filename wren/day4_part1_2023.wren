
import "io" for File

var content = File.read("input.txt")
var total = 0
for (line in content.split("\n")) {
  var trimmed = line.trim()
  if (trimmed == "") continue
  var parts = trimmed.split(":")
  var card = parts[1].split("|")
  var win = []
  for (x in card[0].split(" ")) {
    var t = x.trim()
    if (t != "") win.add(t)
  }
  var matches = 0
  for (x in card[1].split(" ")) {
    var t = x.trim()
    if (t != "" && win.contains(t)) {
      matches = matches + 1
    }
  }
  if (matches > 0) {
    total = total + (1 << (matches - 1))
  }
}
System.print(total)
