import "io" for File

var input = File.read("input.txt")
var lines = input.trim().split("\n")
var validCount = 0

for (line in lines) {
  var words = line.split(" ")
  var wordSet = {}
  var valid = true

  for (word in words) {
    if (wordSet[word]) {
      valid = false
      break
    }
    wordSet[word] = true
  }

  if (valid) validCount = validCount + 1
}

System.print(validCount)