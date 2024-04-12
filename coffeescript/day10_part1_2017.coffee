fs = require 'fs'

# Read input from file
input = fs.readFileSync('input.txt', 'utf8').trim()
lengths = (parseInt(num) for num in input.split(','))

# Initialize variables
list = [0..255]
currentPosition = 0
skipSize = 0

# Perform the knot-tying operations
for length in lengths
  # Reverse the elements
  for i in [0...Math.floor(length / 2)]
    start = (currentPosition + i) % 256
    end = (currentPosition + length - 1 - i) % 256
    [list[start], list[end]] = [list[end], list[start]]

  # Move the current position and increase the skip size
  currentPosition = (currentPosition + length + skipSize) % 256
  skipSize++

# Multiply the first two numbers in the list
result = list[0] * list[1]
console.log result