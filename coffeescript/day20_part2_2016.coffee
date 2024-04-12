fs = require 'fs'

firewall = (input) ->
  allBlockedRanges = []
  for line in input.trim().split "\n"
    parts = line.split('-').map (x) -> parseInt x, 10
    allBlockedRanges.push [parts[0], parts[1]]

  allBlockedRanges.sort (a, b) ->
    if a[0] != b[0]
      a[0] - b[0]
    else
      a[1] - b[1]

  merged = []
  for r in allBlockedRanges
    if merged.length == 0 or merged[merged.length - 1][1] < r[0] - 1
      merged.push r
    else
      merged[merged.length - 1][1] = Math.max merged[merged.length - 1][1], r[1]

  totalAllowed = 0
  for i in [1...merged.length]
    totalAllowed += merged[i][0] - merged[i-1][1] - 1

  totalAllowed

input = fs.readFileSync 'input.txt', 'utf8'
answer = firewall input
console.log answer