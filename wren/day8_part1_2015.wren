import "io" for File

// Read input from file
var input = File.read("input.txt")

// Split input into individual strings
var lines = input.split("\n")

// Initialize counters for code and memory characters
var codeChars = 0
var memoryChars = 0

// Iterate over each string
for (line in lines) {
  // Remove leading/trailing whitespace and quotes
  var str = line.trim()
  str = str[1..-1]

  // Count code characters
  codeChars = codeChars + (str.count + 2) // +2 for quotes

  // Initialize index for unescaped characters
  var i = 0
  var index = 0
  while (index < str.count) {
    // Check for escape sequences
    if (str[index] == "\\") {
      if (str[index + 1] == "x") {
        // \x escape sequence, skip 3 characters
        index = index + 3
      } else {
        // \\ or \" escape sequence, skip 1 character
        index = index + 1
      }
    }
    // Increment memory character count
    memoryChars = memoryChars + 1
    index = index + 1
  }
}

// Print result
System.print(codeChars - memoryChars)