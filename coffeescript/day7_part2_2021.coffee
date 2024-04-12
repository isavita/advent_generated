fs = require 'fs'

calculateNewFuel = (currentPosition, newPosition) ->
  diff = Math.abs currentPosition - newPosition
  (diff * (diff + 1)) // 2

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  positions = data.split(',').map (num) -> parseInt num, 10
  positions.sort (a, b) -> a - b

  minFuel = Number.MAX_SAFE_INTEGER
  for i in [positions[0]..positions[positions.length - 1]]
    fuel = 0
    for pos in positions
      fuel += calculateNewFuel pos, i
    if fuel < minFuel
      minFuel = fuel

  console.log minFuel