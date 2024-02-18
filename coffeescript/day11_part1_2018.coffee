
fs = require('fs')

serial = parseInt(fs.readFileSync('input.txt').toString().trim())

gridSize = 300
grid = []

for y in [0...gridSize]
  row = []
  for x in [0...gridSize]
    rackID = x + 11
    powerLevel = rackID * (y + 1)
    powerLevel += serial
    powerLevel *= rackID
    powerLevel = Math.floor(powerLevel / 100) % 10
    powerLevel -= 5
    row.push powerLevel
  grid.push row

maxPower = -1 << 31
maxX = maxY = 0

for y in [0...gridSize - 2]
  for x in [0...gridSize - 2]
    totalPower = 0
    for dy in [0...3]
      for dx in [0...3]
        totalPower += grid[y + dy][x + dx]
    if totalPower > maxPower
      maxPower = totalPower
      maxX = x + 1
      maxY = y + 1

console.log "#{maxX},#{maxY}"
