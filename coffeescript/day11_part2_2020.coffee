fs = require 'fs'

Point = (x, y) ->
  @x = x
  @y = y

directions = [
  new Point(-1, -1), new Point(0, -1), new Point(1, -1),
  new Point(-1, 0), new Point(1, 0),
  new Point(-1, 1), new Point(0, 1), new Point(1, 1)
]

simulateSeatingPartTwo = (seatingArea) ->
  rows = seatingArea.length
  cols = seatingArea[0].length
  newSeatingArea = []
  for i in [0...rows]
    newSeatingArea.push Array.from(seatingArea[i])
  stabilized = true

  for i in [0...rows]
    for j in [0...cols]
      switch seatingArea[i][j]
        when 'L'
          if countVisibleOccupied(seatingArea, i, j) == 0
            newSeatingArea[i][j] = '#'
            stabilized = false
        when '#'
          if countVisibleOccupied(seatingArea, i, j) >= 5
            newSeatingArea[i][j] = 'L'
            stabilized = false

  [newSeatingArea, stabilized]

countVisibleOccupied = (seatingArea, row, col) ->
  count = 0
  for dir in directions
    r = row + dir.y
    c = col + dir.x
    while r >= 0 and r < seatingArea.length and c >= 0 and c < seatingArea[0].length
      if seatingArea[r][c] == 'L'
        break
      if seatingArea[r][c] == '#'
        count++
        break
      r += dir.y
      c += dir.x
  count

countOccupiedSeats = (seatingArea) ->
  count = 0
  for row in seatingArea
    for seat in row
      count++ if seat == '#'
  count

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error opening file:", err
    return

  seatingArea = data.split("\n").map (line) -> line.split ''

  stabilized = false
  while not stabilized
    [seatingArea, stabilized] = simulateSeatingPartTwo(seatingArea)

  console.log countOccupiedSeats(seatingArea)