import "io" for File

// Read input from file
var input = File.read("input.txt")

// Split input into individual frequency changes
var changes = input.split("
")

// Initialize frequency to 0
var frequency = 0

// Iterate over each frequency change
for (change in changes) {
  // Parse the change as an integer
  var delta = Num.fromString(change)

  // Apply the change to the frequency
  frequency = frequency + delta
}

// Print the resulting frequency
System.print(frequency)