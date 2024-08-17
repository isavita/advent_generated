import "io" for File

var file = File.read("input.txt")
var lines = file.split("\n")

var message = ""
var correctedMessage = ""

// Calculate the most common character for each position
for (i in 0..lines[0].count-1) {
  var charCount = {}
  for (line in lines) {
    var char = line[i]
    if (charCount[char] == null) {
      charCount[char] = 1
    } else {
      charCount[char] = charCount[char] + 1
    }
  }
  var mostCommonChar = ""
  var maxCount = 0
  for (char in charCount.keys) {
    if (charCount[char] > maxCount) {
      maxCount = charCount[char]
      mostCommonChar = char
    }
  }
  message = message + mostCommonChar
}

// Calculate the least common character for each position
for (i in 0..lines[0].count-1) {
  var charCount = {}
  for (line in lines) {
    var char = line[i]
    if (charCount[char] == null) {
      charCount[char] = 1
    } else {
      charCount[char] = charCount[char] + 1
    }
  }
  var leastCommonChar = ""
  var minCount = 1000
  for (char in charCount.keys) {
    if (charCount[char] < minCount) {
      minCount = charCount[char]
      leastCommonChar = char
    }
  }
  correctedMessage = correctedMessage + leastCommonChar
}

System.print("Part 1: " + message)
System.print("Part 2: " + correctedMessage)