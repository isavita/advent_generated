fs = require 'fs'

simulateSeating = (seatingArea) ->
  rows = seatingArea.length
  cols = seatingArea[0].length
  newSeatingArea = JSON.parse JSON.stringify seatingArea
  stabilized = true

  for i in [0...rows]
    for j in [0...cols]
      switch seatingArea[i][j]
        when 'L'
          if countAdjacentOccupied(seatingArea, i, j) == 0
            newSeatingArea[i][j] = '#'
            stabilized = false
        when '#'
          if countAdjacentOccupied(seatingArea, i, j) >= 4
            newSeatingArea[i][j] = 'L'
            stabilized = false

  [newSeatingArea, stabilized]

countAdjacentOccupied = (seatingArea, row, col) ->
  count = 0
  for i in [row-1..row+1]
    for j in [col-1..col+1]
      continue if i == row && j == col
      if i >= 0 && i < seatingArea.length && j >= 0 && j < seatingArea[0].length
        count++ if seatingArea[i][j] == '#'
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

  seatingArea = (line.split '' for line in data.trim().split '\n')
  stabilized = false

  until stabilized
    [seatingArea, stabilized] = simulateSeating seatingArea

  console.log countOccupiedSeats seatingArea