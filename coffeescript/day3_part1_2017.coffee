
fs = require 'fs'

# Step 1: Read input
input = -> fs.readFileSync('input.txt', 'utf8').trim()

# Step 2: Find the square
findSquare = (target) ->
  sideLength = Math.ceil(Math.sqrt(target))
  if sideLength % 2 == 0 then sideLength++
  sideLength

# Step 3: Find distance to the nearest middle point
findDistanceToMiddle = (target, sideLength) ->
  maxValue = sideLength * sideLength
  stepsFromEdge = (sideLength - 1) / 2
  distanceToMiddle = null

  for i in [0...4]
    middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i
    distance = Math.abs(target - middlePoint)
    if distance < distanceToMiddle? or i == 0 then distanceToMiddle = distance

  distanceToMiddle

# Step 4: Calculate Manhattan Distance
calculateManhattanDistance = (target, distanceToMiddle, stepsFromEdge) ->
  stepsFromEdge + distanceToMiddle

# Main function
main = ->
  target = parseInt(input())
  sideLength = findSquare(target)
  distanceToMiddle = findDistanceToMiddle(target, sideLength)
  stepsFromEdge = (sideLength - 1) / 2
  manhattanDistance = calculateManhattanDistance(target, distanceToMiddle, stepsFromEdge)
  console.log manhattanDistance

main()
