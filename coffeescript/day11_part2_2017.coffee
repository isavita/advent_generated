fs = require 'fs'

abs = (x) -> if x < 0 then -x else x

max = (a, b) -> if a > b then a else b

distance = (x, y, z) -> Math.floor (abs(x) + abs(y) + abs(z)) / 2

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "File reading error", err
    return

  directions = data.trim().split ','

  x = y = z = 0
  maxDistance = 0

  for dir in directions
    switch dir
      when "n" then y++; z--
      when "ne" then x++; z--
      when "se" then x++; y--
      when "s" then y--; z++
      when "sw" then x--; z++
      when "nw" then x--; y++

    # Calculate the current distance
    curDistance = distance x, y, z
    maxDistance = max maxDistance, curDistance

  console.log maxDistance