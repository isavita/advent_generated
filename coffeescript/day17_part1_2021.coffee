fs = require 'fs'

isMovingAway = (xPos, yPos, xVel, yVel, xMin, xMax, yMin, yMax) ->
  return true if xPos < xMin and xVel < 0
  return true if xPos > xMax and xVel > 0
  return true if yPos < yMin and yVel < 0
  false

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  line = data.trim()
  parts = line.split ', '
  xRange = parts[0].slice(15).split '..'
  yRange = parts[1].slice(2).split '..'

  xMin = parseInt xRange[0]
  xMax = parseInt xRange[1]
  yMin = parseInt yRange[0]
  yMax = parseInt yRange[1]

  maxY = -Infinity
  for xVel in [-1000..1000]
    for yVel in [-1000..1000]
      xPos = 0
      yPos = 0
      curXVel = xVel
      curYVel = yVel
      highestY = yPos

      while true
        xPos += curXVel
        yPos += curYVel

        if xPos >= xMin and xPos <= xMax and yPos >= yMin and yPos <= yMax
          maxY = Math.max maxY, highestY
          break

        break if isMovingAway xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax

        curXVel-- if curXVel > 0
        curXVel++ if curXVel < 0
        curYVel--
        highestY = Math.max highestY, yPos

  console.log maxY