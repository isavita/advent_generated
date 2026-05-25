
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

var clean = Fn.new { |s|
  while (s.count > 0 && (s.endsWith(":") || s.endsWith(","))) {
    s = s[0...s.count-1]
  }
  return s
}

var content = File.read("input.txt")
var lines = content.split("\n")

var part1 = null
var part2 = null

for (line in lines) {
  var trimmed = line.trim()
  if (trimmed == "") continue
  var parts = trimmed.split(" ")
  var sueId = clean.call(parts[1])
  
  var isPart1 = true
  var isPart2 = true
  
  var i = 2
  while (i < parts.count) {
    var key = clean.call(parts[i])
    var val = Num.fromString(clean.call(parts[i+1]))
    
    if (target[key] != val) {
      isPart1 = false
    }
    
    if (key == "cats" || key == "trees") {
      if (val <= target[key]) isPart2 = false
    } else if (key == "pomeranians" || key == "goldfish") {
      if (val >= target[key]) isPart2 = false
    } else {
      if (val != target[key]) isPart2 = false
    }
    
    i = i + 2
  }
  
  if (isPart1) part1 = sueId
  if (isPart2) part2 = sueId
}

System.print("Part 1 (The Sue that matches exactly): %(part1)")
System.print("Part 2 (The Sue based on range adjustments): %(part2)")
