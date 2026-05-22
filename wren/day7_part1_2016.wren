
import "io" for File

var containsABBA = Fn.new { |s|
  if (s.count < 4) return false
  for (i in 0...(s.count - 3)) {
    if (s[i] != s[i+1] && s[i] == s[i+3] && s[i+1] == s[i+2]) return true
  }
  return false
}

var solveLine = Fn.new { |line|
  var inside = false
  var hasOutside = false
  var current = ""
  for (i in 0...line.count) {
    var c = line[i]
    if (c == "[") {
      if (containsABBA.call(current)) {
        if (inside) return false
        hasOutside = true
      }
      current = ""
      inside = true
    } else if (c == "]") {
      if (containsABBA.call(current)) {
        if (inside) return false
        hasOutside = true
      }
      current = ""
      inside = false
    } else {
      current = current + c
    }
  }
  if (containsABBA.call(current)) {
    if (inside) return false
    hasOutside = true
  }
  return hasOutside
}

var content = File.read("input.txt")
var tlsCount = 0
for (line in content.split("\n")) {
  var trimmed = line.trim()
  if (trimmed.count > 0 && solveLine.call(trimmed)) {
    tlsCount = tlsCount + 1
  }
}
System.print(tlsCount)
