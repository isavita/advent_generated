
import "io" for File

var priority = Fn.new { |c|
  var code = c.bytes[0]
  if (code >= 97 && code <= 122) {
    return code - 97 + 1
  } else {
    return code - 65 + 27
  }
}

var part1 = Fn.new { |lines|
  var total = 0
  for (line in lines) {
    var len = line.count
    var mid = (len / 2).floor
    var first = line[0...mid]
    var second = line[mid...len]
    var common = null
    for (ch in first) {
      if (second.contains(ch)) {
        common = ch
        break
      }
    }
    if (common != null) {
      total = total + priority.call(common)
    }
  }
  return total
}

var part2 = Fn.new { |lines|
  var total = 0
  var i = 0
  while (i < lines.count) {
    var a = lines[i]
    var b = lines[i+1]
    var c = lines[i+2]
    var common = null
    for (ch in a) {
      if (b.contains(ch) && c.contains(ch)) {
        common = ch
        break
      }
    }
    if (common != null) {
      total = total + priority.call(common)
    }
    i = i + 3
  }
  return total
}

var content = File.read("input.txt").trim()
var lines = content.split("\n")
System.print("Part 1: %(part1.call(lines))")
System.print("Part 2: %(part2.call(lines))")
