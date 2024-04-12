fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  serial = parseInt data.trim()
  gridSize = 300
  grid = (Array(gridSize).fill().map -> Array(gridSize).fill(0))

  # Calculate power level for each cell
  for y in [0...gridSize]
    for x in [0...gridSize]
      rackID = x + 11
      powerLevel = rackID * (y + 1)
      powerLevel += serial
      powerLevel *= rackID
      powerLevel = Math.floor(powerLevel / 100) % 10
      powerLevel -= 5
      grid[y][x] = powerLevel

  maxPower = -Infinity
  maxX = maxY = maxSize = 0

  for size in [1..gridSize+1]
    for y in [0...gridSize-size+1]
      for x in [0...gridSize-size+1]
        totalPower = 0
        for dy in [0...size]
          for dx in [0...size]
            totalPower += grid[y+dy][x+dx]

        if totalPower > maxPower
          maxPower = totalPower
          [maxX, maxY, maxSize] = [x+1, y+1, size]

  console.log "#{maxX},#{maxY},#{maxSize}"