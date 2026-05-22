
import "io" for File

var lines = File.read("input.txt").replace("\r", "").split("\n")
var total = 0
var group = {}
for (line in lines) {
  if (line == "") {
    total = total + group.count
    group = {}
  } else {
    for (char in line) group[char] = true
  }
}
total = total + group.count
System.print(total)
