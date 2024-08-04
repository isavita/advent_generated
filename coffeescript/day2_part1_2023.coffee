fs = require 'fs'

fileContent = fs.readFileSync 'input.txt', 'utf8'
regex = /Game (\d+): (.+)/
cubeRegex = /(\d+) (red|green|blue)/g
totalSum = 0

lines = fileContent.split '\n'
for line in lines
  matches = regex.exec line

  if matches?.length == 3
    gameId = parseInt matches[1]
    rounds = matches[2].split ';'
    isValid = true

    for round in rounds
      cubes = []
      while cubeMatch = cubeRegex.exec round
        cubes.push cubeMatch

      red = green = blue = 0

      for cube in cubes
        count = parseInt cube[1]
        switch cube[2]
          when 'red' then red += count
          when 'green' then green += count
          when 'blue' then blue += count

        if red > 12 || green > 13 || blue > 14
          isValid = false
          break

      if not isValid
        break

    if isValid
      totalSum += gameId

console.log totalSum