
import "io" for File

var content = File.read("input.txt").replace("\n", "").replace("\r", "")
var total = 0
for (step in content.split(",")) {
  if (step != "") {
    var h = 0
    for (b in step.bytes) {
      h = ((h + b) * 17) % 256
    }
    total = total + h
  }
}
System.print(total)
