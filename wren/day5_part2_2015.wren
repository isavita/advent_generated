import "io" for File

var isNice = Fn.new { |s|
  // Check for at least three vowels
  var vowels = "aeiou"
  var vowelCount = 0
  for (char in s) {
    if (vowels.contains(char)) vowelCount = vowelCount + 1
  }
  if (vowelCount < 3) return false

  // Check for at least one double letter
  for (i in 0..s.count-2) {
    if (s[i] == s[i+1]) return true
  }
  return false
}

var isNaughty = Fn.new { |s|
  var naughtyStrings = ["ab", "cd", "pq", "xy"]
  for (ns in naughtyStrings) {
    if (s.contains(ns)) return true
  }
  return false
}

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var niceCount = 0

  for (line in lines) {
    var s = line.trim()
    if (isNice.call(s) && !isNaughty.call(s)) niceCount = niceCount + 1
  }

  System.print(niceCount)
}

main.call()