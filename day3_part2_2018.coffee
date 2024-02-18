
fabric = []
for i in [0...1000]
  fabric.push Array.apply(null, Array(1000)).map (x) -> 0

claims = []
fs = require('fs')
input = fs.readFileSync('input.txt').toString().split('\n')
for line in input
  parts = line.split(' ')
  id = parseInt(parts[0][1...])
  x = parseInt(parts[2].split(',')[0])
  y = parseInt(parts[2].split(',')[1][0...-1])
  width = parseInt(parts[3].split('x')[0])
  height = parseInt(parts[3].split('x')[1])
  for yy in [y...y+height]
    for xx in [x...x+width]
      fabric[yy][xx]++
  claims.push {id, x, y, width, height}

for claim in claims
  overlap = false
  for y in [claim.y...claim.y+claim.height]
    for x in [claim.x...claim.x+claim.width]
      if fabric[y][x] > 1
        overlap = true
        break
    if overlap
      break
  if not overlap
    console.log claim.id
    process.exit()
