import "io" for File

// Read input from file
var input = File.read("input.txt")

// Split input into individual strings
var lines = input.split("\n")

// Find the length of the longest line
var maxLength = 0
for (line in lines) {
  if (line.count > maxLength) {
    maxLength = line.count
  }
}

// Initialize a map to store character frequencies for each position
var freqs = {}
for (i in 0..maxLength - 1) {
  freqs[i] = {}
}

// Count character frequencies for each position
for (line in lines) {
  for (i in 0..line.count - 1) {
    var char = line[i]
    if (!freqs[i][char]) {
      freqs[i][char] = 0
    }
    freqs[i][char] = freqs[i][char] + 1
  }
}

// Find the most frequent character for each position
var message = ""
for (i in 0..maxLength - 1) {
  var maxCount = 0
  var mostFrequentChar = ""
  for (char in freqs[i].keys) {
    if (freqs[i][char] > maxCount) {
      maxCount = freqs[i][char]
      mostFrequentChar = char
    }
  }
  message = message + mostFrequentChar
}

// Print the error-corrected message
System.print(message)