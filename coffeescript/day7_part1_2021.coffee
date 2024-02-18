
fs = require 'fs'

readInputFile = (callback) ->
  fs.readFile 'input.txt', 'utf8', (err, data) ->
    if err?
      console.error err
      return
    callback data.split(',').map (numStr) -> parseInt(numStr)

readInputFile (positions) ->
  sortedPositions = positions.slice().sort (a, b) -> a - b

  minFuel = Number.MAX_SAFE_INTEGER
  for i in [sortedPositions[0]...sortedPositions[sortedPositions.length - 1]]
    fuel = 0
    for pos in positions
      fuel += calculateFuel(pos, i)
    minFuel = Math.min minFuel, fuel

  console.log minFuel

calculateFuel = (currentPosition, newPosition) ->
  Math.abs(currentPosition - newPosition)
