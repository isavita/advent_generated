import "io" for File

// Read input from file
var input = File.read("input.txt")

// Initialize starting position and a set to track visited houses
var x = 0
var y = 0
var visited = {}

// Mark the starting house as visited
visited["0,0"] = true

// Process each direction from the input
for (char in input) {
  if (char == "^") {
    y = y + 1
  } else if (char == "v") {
    y = y - 1
  } else if (char == ">") {
    x = x + 1
  } else if (char == "<") {
    x = x - 1
  }

  // Mark the current house as visited
  visited["%(x),%(y)"] = true
}

// Print the number of unique houses visited
System.print(visited.count)