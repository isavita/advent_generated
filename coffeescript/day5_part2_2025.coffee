
fs = require 'fs'

main = ->
  data = fs.readFileSync 'input.txt', 'utf8'
  lines = data.split /\r?\n/
  ranges = []
  for line in lines
    line = line.trim()
    break if line is ''
    [a, b] = line.split '-'
    continue unless a? and b?
    min = parseInt a.trim(), 10
    max = parseInt b.trim(), 10
    continue if isNaN min or isNaN max
    if min > max then [min, max] = [max, min]
    ranges.push [min, max]

  return console.log 'Total fresh IDs: 0' if ranges.length is 0

  ranges.sort (x, y) -> if x[0] is y[0] then x[1] - y[1] else x[0] - y[0]

  total = 0
  [curMin, curMax] = ranges[0]

  for i in [1...ranges.length]
    [mn, mx] = ranges[i]
    if mn <= curMax
      curMax = mx if mx > curMax
    else
      total += curMax - curMin + 1
      [curMin, curMax] = [mn, mx]

  total += curMax - curMin + 1
  console.log "Total fresh IDs: #{total}"

main()
