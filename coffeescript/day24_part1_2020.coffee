fs = require 'fs'

Coordinate = (q, r) ->
  @q = q
  @r = r

directions =
  e:  new Coordinate(1, 0)
  se: new Coordinate(0, 1)
  sw: new Coordinate(-1, 1)
  w:  new Coordinate(-1, 0)
  nw: new Coordinate(0, -1)
  ne: new Coordinate(1, -1)

blackTiles = {}

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  lines = data.split('\n')

  for line in lines
    coord = new Coordinate(0, 0)

    i = 0
    while i < line.length
      switch line[i]
        when 'e', 'w'
          dir = line[i]
        when 'n', 's'
          dir = line.substring(i, i + 2)
          i++

      move = directions[dir]
      coord.q += move.q
      coord.r += move.r
      i++

    key = "#{coord.q},#{coord.r}"
    blackTiles[key] = not blackTiles[key]

  count = 0
  for key, black of blackTiles
    count++ if black

  console.log count