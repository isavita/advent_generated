
fs = require('fs')

parseRange = (r) ->
  parts = r.split('-')
  [parseInt(parts[0]), parseInt(parts[1])]

count = 0

input = fs.readFileSync('input.txt', 'utf8').split('\n')

for line in input
  continue if line == ''
  ranges = line.split(',')
  if ranges.length != 2
    continue
  [start1, end1] = parseRange(ranges[0])
  [start2, end2] = parseRange(ranges[1])
  if (start1 <= start2 and end1 >= end2) or (start2 <= start1 and end2 >= end1)
    count++

console.log(count)
