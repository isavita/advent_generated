
fs = require 'fs'

try
  input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
catch error
  console.error "Failed to read input file: #{error}"
  process.exit(1)

safeReportCount = 0

for line in input
  levels = line.split(/\s+/).map(Number)
  if levels.length < 2
    continue

  firstDiff = levels[1] - levels[0]
  if firstDiff is 0
    continue

  isIncreasing = firstDiff > 0
  safe = true

  for i in [0...levels.length - 1]
    diff = levels[i + 1] - levels[i]
    if diff is 0
      safe = false
      break
    if (isIncreasing and diff <= 0) or (not isIncreasing and diff >= 0)
      safe = false
      break
    absDiff = Math.abs(diff)
    if absDiff < 1 or absDiff > 3
      safe = false
      break
  if safe
    safeReportCount++

console.log safeReportCount
