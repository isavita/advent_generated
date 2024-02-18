
fs = require 'fs'

# Step 1: Read Input
try
  data = fs.readFileSync('input.txt', 'utf8')
catch err
  console.error 'File reading error', err
  process.exit(1)

lines = data.trim().split('\n')
offsets = lines.map (line) -> parseInt(line, 10)

# Step 2: Initialize Variables
index = 0
steps = 0

# Step 3: Navigate Maze
while index >= 0 and index < offsets.length
  # Fetch the jump offset at the current index
  jump = offsets[index]

  # Step 4: Update Offset
  if jump >= 3
    offsets[index]--
  else
    offsets[index]++

  # Move to the new index
  index += jump

  # Increment steps counter
  steps++

# Step 6: Output
console.log steps
