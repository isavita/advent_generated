fs = require 'fs'

class IPRange
  constructor: (@start, @end) ->

readIPRanges = (filename) ->
  ranges = []
  data = fs.readFileSync(filename, 'utf8')
  lines = data.trim().split '\n'
  for line in lines
    parts = line.split '-'
    start = parseInt(parts[0], 10)
    end = parseInt(parts[1], 10)
    ranges.push new IPRange(start, end)
  ranges

findUnblockedIP = (ranges) ->
  ranges.sort (a, b) -> a.start - b.start
  currentIP = 0
  for r in ranges
    return currentIP if r.start > currentIP
    currentIP = r.end + 1 if r.end >= currentIP
  currentIP

ipRanges = readIPRanges "input.txt"
unblockedIP = findUnblockedIP ipRanges
console.log unblockedIP