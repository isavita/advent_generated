import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
var checksum = 0
for (line in lines) {
  line = line.trim()
  if (line == "") continue
  var normalized = line.replace("\t", " ")
  var words = normalized.split(" ")
  var minVal = null
  var maxVal = null
  for (word in words) {
    if (word == "") continue
    var num = Num.fromString(word)
    if (minVal == null) {
      minVal = num
      maxVal = num
    } else {
      if (num < minVal) minVal = num
      if (num > maxVal) maxVal = num
    }
  }
  if (minVal != null) checksum = checksum + (maxVal - minVal)
}
System.print(checksum)