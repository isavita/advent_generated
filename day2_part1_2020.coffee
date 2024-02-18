
# Read input from input.txt file
fs = require('fs')
input = fs.readFileSync('input.txt', 'utf8')

# Split input into lines
lines = input.split('\n')

# Initialize valid password count to 0
validCount = 0

# Iterate over lines
for line in lines
  # Split line into policy and password
  parts = line.split(':')
  policy = parts[0].trim()
  password = parts[1].trim()

  # Parse policy to get min, max, and char
  minMaxChar = policy.split(' ')
  min = parseInt(minMaxChar[0].split('-')[0])
  max = parseInt(minMaxChar[0].split('-')[1])
  char = minMaxChar[1][0]

  # Count occurrences of char in password
  count = (c for c in password when c == char).length

  # Check if password is valid
  if count >= min and count <= max
    validCount++

# Print valid password count
console.log validCount
