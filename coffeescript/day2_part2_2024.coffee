
fs = require 'fs'

isSafeReport = (levels) ->
  return false if levels.length < 2
  firstDiff = levels[1] - levels[0]
  return false if firstDiff is 0
  isIncreasing = firstDiff > 0
  for i in [0...levels.length - 1]
    diff = levels[i+1] - levels[i]
    return false if diff is 0
    return false if (isIncreasing and diff <= 0) or (not isIncreasing and diff >= 0)
    absDiff = Math.abs(diff)
    return false if absDiff < 1 or absDiff > 3
  true

isSafeWithOneRemoval = (levels) ->
  for i in [0...levels.length]
    modifiedLevels = levels.slice(0, i).concat(levels.slice(i + 1))
    if isSafeReport(modifiedLevels)
      return true
  false

solve = ->
  try
    data = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
    safeReportCount = 0
    for line in data
      levels = line.trim().split(/\s+/).map(Number)
      if isSafeReport(levels) or isSafeWithOneRemoval(levels)
        safeReportCount++
    console.log safeReportCount
  catch error
    console.error "Error:", error

solve()
