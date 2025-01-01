
fs = require 'fs'

try
  data = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
catch error
  console.error "Failed to read input.txt: #{error.message}"
  process.exit(1)

left = []
right = []

for line, index in data
  parts = line.trim().split(/\s+/)
  if parts.length != 2
    console.error "Invalid input format at line #{index + 1}: expected 2 numbers, got #{parts.length}"
    process.exit(1)
  try
    left.push parseInt(parts[0], 10)
    right.push parseInt(parts[1], 10)
  catch error
    console.error "Invalid number at line #{index + 1}: #{error.message}"
    process.exit(1)

rightCount = {}
for num in right
  rightCount[num] = (rightCount[num] or 0) + 1

similarityScore = 0
for num in left
  if rightCount[num]?
    similarityScore += num * rightCount[num]

console.log similarityScore
