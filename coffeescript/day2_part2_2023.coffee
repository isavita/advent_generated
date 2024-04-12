fs = require 'fs'

calculatePower = (input) ->
  totalPower = 0
  gameRegex = /Game (\d+): (.+)/
  cubeRegex = /(\d+) (red|green|blue)/g

  for line in input.split '\n'
    gameMatch = gameRegex.exec line
    if gameMatch?
      rounds = gameMatch[2].split ';'
      maxRed = maxGreen = maxBlue = 0

      for round in rounds
        red = green = blue = 0
        while cubeMatch = cubeRegex.exec round
          count = parseInt cubeMatch[1]
          switch cubeMatch[2]
            when 'red' then red += count
            when 'green' then green += count
            when 'blue' then blue += count
        maxRed = Math.max maxRed, red
        maxGreen = Math.max maxGreen, green
        maxBlue = Math.max maxBlue, blue

      power = maxRed * maxGreen * maxBlue
      totalPower += power

  console.log totalPower

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log 'Error reading file:', err
    return
  calculatePower data