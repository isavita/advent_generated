fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  [depth, target] = parseInput data
  cave = makeCaveSystem depth, target
  riskLevel = calculateRiskLevel cave, target
  console.log riskLevel

parseInput = (data) ->
  lines = data.split '\n'
  depth = parseInt lines[0].split(' ')[1]
  coords = lines[1].split(' ')[1].split(',')
  x = parseInt coords[0]
  y = parseInt coords[1]
  [depth, [x, y]]

makeCaveSystem = (depth, target) ->
  cave = Array(target[1] + 1).fill().map -> Array(target[0] + 1).fill(0)
  for y in [0..target[1]]
    for x in [0..target[0]]
      geologicIndex = if x == 0 and y == 0 or x == target[0] and y == target[1]
        0
      else if y == 0
        x * 16807
      else if x == 0
        y * 48271
      else
        cave[y][x-1] * cave[y-1][x]
      cave[y][x] = (geologicIndex + depth) % 20183
  cave

calculateRiskLevel = (cave, target) ->
  riskLevel = 0
  for y in [0..target[1]]
    for x in [0..target[0]]
      riskLevel += cave[y][x] % 3
  riskLevel