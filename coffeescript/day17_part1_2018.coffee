
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim()
lines = input.split('\n')

ground = [['+']]
maxX = 0
minX = 0
maxY = 0
minY = 20
xOffset = 500
yOffset = 0

strToInt = (s) -> parseInt(s)

for line in lines
  split = line.split(/[=, .]+/)
  if split[0] == 'x'
    x = strToInt(split[1]) - xOffset
    y1 = strToInt(split[3]) - yOffset
    y2 = strToInt(split[4]) - yOffset

    while x >= maxX
      maxX++
      for row in ground
        row.push('.')
    while x <= minX
      minX--
      for row in ground
        row.unshift('.')
    while y2 > maxY
      maxY++
      ground.push(new Array(ground[0].length).fill('.'))
    minY = Math.min(minY, y1)
    for i in [y1..y2]
      ground[i][x - minX] = '#'
  else
    y = strToInt(split[1]) - yOffset
    x1 = strToInt(split[3]) - xOffset
    x2 = strToInt(split[4]) - xOffset

    while y > maxY
      maxY++
      ground.push(new Array(ground[0].length).fill('.'))
    while x2 >= maxX
      maxX++
      for row in ground
        row.push('.')
    while x1 <= minX
      minX--
      for row in ground
        row.unshift('.')
    for i in [x1..x2]
      ground[y][i - minX] = '#'
    minY = Math.min(minY, y)

waterCount = 0
flowCount = 0
roundLimit = 200000

while ground[1][-minX] != '|' and waterCount < roundLimit
  canMove = true
  x = -minX
  y = 1
  tryLeft = 0
  while canMove
    if y + 1 > maxY or ground[y + 1][x] == '|'
      ground[y][x] = '|'
      canMove = false
      if y >= minY
        flowCount++
    else if ground[y + 1][x] == '.'
      y++
      tryLeft = 0
    else if ground[y + 1][x] == '#' or ground[y + 1][x] == '~'
      if (tryLeft == 1 and ground[y][x - 1] == '|') or
         (tryLeft == 2 and ground[y][x + 1] == '|') or
         (ground[y][x + 1] == '|' and ground[y][x - 1] != '.') or
         (ground[y][x + 1] != '.' and ground[y][x - 1] == '|')
        ground[y][x] = '|'
        flowCount++
        canMove = false
        i = x + 1
        while ground[y][i] == '~'
          ground[y][i] = '|'
          waterCount--
          flowCount++
          i++
        i = x - 1
        while ground[y][i] == '~'
          ground[y][i] = '|'
          waterCount--
          flowCount++
          i--
      else if (tryLeft == 0 and ground[y][x - 1] == '.') or
              (tryLeft == 1 and ground[y][x - 1] == '.')
        x--
        tryLeft = 1
      else if (tryLeft == 0 and ground[y][x + 1] == '.') or
              (tryLeft == 2 and ground[y][x + 1] == '.')
        x++
        tryLeft = 2
      else
        canMove = false
        ground[y][x] = '~'
        waterCount++

console.log flowCount + waterCount
