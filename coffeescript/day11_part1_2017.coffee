fs = require 'fs'

abs = (x) -> if x < 0 then -x else x

max = (a, b) -> if a > b then a else b

distance = (x, y, z) -> (abs(x) + abs(y) + abs(z)) // 2

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log 'File reading error', err
    return

  directions = data.trim().split ','

  x = y = z = 0
  maxDistance = 0

  for dir in directions
    switch dir
      when 'n'
        y++
        z--
      when 'ne'
        x++
        z--
      when 'se'
        x++
        y--
      when 's'
        y--
        z++
      when 'sw'
        x--
        z++
      when 'nw'
        x--
        y++

    # Calculate the current distance
    curDistance = distance x, y, z
    maxDistance = max maxDistance, curDistance

  console.log distance x, y, z