import "io" for File

// Read input from file
var input = File.read("input.txt")

// Split input into individual commands
var commands = input.split("\n")

// Initialize counters for horizontal position and depth
var horizontal = 0
var depth = 0

// Iterate over each command
for (command in commands) {
  // Split command into direction and distance
  var parts = command.split(" ")
  var direction = parts[0]
  var distance = Num.fromString(parts[1])

  // Update horizontal position or depth based on direction
  if (direction == "forward") {
    horizontal = horizontal + distance
  } else if (direction == "down") {
    depth = depth + distance
  } else if (direction == "up") {
    depth = depth - distance
  }
}

// Print result
System.print(horizontal * depth)