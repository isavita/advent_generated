import "io" for File

var countTwosAndThrees = Fn.new { |id|
  var charCount = {}
  for (char in id) {
    if (charCount[char] == null) {
      charCount[char] = 1
    } else {
      charCount[char] = charCount[char] + 1
    }
  }

  var hasTwos = false
  var hasThrees = false
  for (count in charCount.values) {
    if (count == 2) hasTwos = true
    if (count == 3) hasThrees = true
  }
  return [hasTwos, hasThrees]
}

var input = File.read("input.txt")
var lines = input.split("\n")

var twoCount = 0
var threeCount = 0

for (line in lines) {
  var result = countTwosAndThrees.call(line)
  var hasTwos = result[0]
  var hasThrees = result[1]
  if (hasTwos) twoCount = twoCount + 1
  if (hasThrees) threeCount = threeCount + 1
}

var checksum = twoCount * threeCount
System.print(checksum)