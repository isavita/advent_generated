
fs = require('fs')

grid = {}

input = fs.readFileSync('input.txt', 'utf8')
lines = input.split('\n')

for line in lines
  coords = line.split(' -> ')
  start = coords[0].split(',').map(Number)
  end = coords[1].split(',').map(Number)

  [x1, y1] = start
  [x2, y2] = end

  if x1 == x2
    if y1 > y2
      [y1, y2] = [y2, y1]
    for y in [y1...y2+1]
      grid[[x1, y]] = (grid[[x1, y]] or 0) + 1
  else if y1 == y2
    if x1 > x2
      [x1, x2] = [x2, x1]
    for x in [x1...x2+1]
      grid[[x, y1]] = (grid[[x, y1]] or 0) + 1

overlap_count = (value for key, value of grid when value > 1).length
console.log overlap_count
