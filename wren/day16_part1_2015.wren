
import "io" for File

var target = {
  "children": 3,
  "cats": 7,
  "samoyeds": 2,
  "pomeranians": 3,
  "akitas": 0,
  "vizslas": 0,
  "goldfish": 5,
  "trees": 3,
  "cars": 2,
  "perfumes": 1
}

var content = File.read("input.txt")
for (line in content.split("\n")) {
  if (line == "") continue
  var cleaned = line.replace(":", " ").replace(",", " ")
  var parts = cleaned.split(" ").where { |s| s != "" }.toList
  var sue = parts[1]
  var match = true
  var i = 2
  while (i < parts.count) {
    var key = parts[i]
    var val = Num.fromString(parts[i+1])
    if (target.containsKey(key) && target[key] != val) {
      match = false
      break
    }
    i = i + 2
  }
  if (match) {
    System.print(sue)
    break
  }
}
