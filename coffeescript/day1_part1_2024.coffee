
fs = require 'fs'

try
  data = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
catch error
  console.error "Failed to read input.txt: #{error.message}"
  process.exit(1)

left = []
right = []

for line, i in data
  parts = line.trim().split(/\s+/)
  if parts.length != 2
    console.error "Invalid input format at line #{i + 1}: expected 2 numbers, got #{parts.length}"
    process.exit(1)
  try
    left.push parseInt(parts[0], 10)
    right.push parseInt(parts[1], 10)
  catch error
    console.error "Invalid number at line #{i + 1}: #{error.message}"
    process.exit(1)

if left.length != right.length
  console.error "Lists have different lengths: left has #{left.length} elements, right has #{right.length} elements"
  process.exit(1)

left.sort (a, b) -> a - b
right.sort (a, b) -> a - b

totalDistance = 0
for i in [0...left.length]
  totalDistance += Math.abs(left[i] - right[i])

console.log totalDistance
