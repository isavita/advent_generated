
# Read input from file
fs = require 'fs'
input = fs.readFileSync('input.txt', 'utf8').trim().split(/\s+/)

# Convert input strings to integers
banks = input.map (num) -> parseInt(num)

# Initialize variables
seen = {}
cycles = 0

# Redistribution loop
while true
  # Convert current banks state to string to store in map
  state = JSON.stringify banks

  # Check for repeats
  if (prevCycle = seen[state])?
    console.log "The size of the loop is #{cycles - prevCycle}"
    return
  seen[state] = cycles

  # Find the bank with most blocks
  maxIndex = 0
  for i in [1...banks.length]
    if banks[i] > banks[maxIndex]
      maxIndex = i

  # Perform redistribution
  blocks = banks[maxIndex]
  banks[maxIndex] = 0
  for i in [1...blocks + 1]
    banks[(maxIndex + i) % banks.length]++

  # Increment cycle counter
  cycles++
