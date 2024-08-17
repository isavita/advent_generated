import "io" for File

var isNice = Fn.new { |str|
  var vowels = 0
  var hasDouble = false
  var hasBadSubstring = false

  for (i in 0..str.count - 1) {
    if ("aeiou".contains(str[i])) vowels = vowels + 1
    if (i < str.count - 1 && str[i] == str[i + 1]) hasDouble = true
    if (i < str.count - 1 && ["ab", "cd", "pq", "xy"].any { |s| str[i..i + 1] == s }) hasBadSubstring = true
  }

  return vowels >= 3 && hasDouble && !hasBadSubstring
}

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var niceCount = 0

  for (line in lines) {
    if (isNice.call(line.trim())) niceCount = niceCount + 1
  }

  System.print(niceCount)
}

main.call()