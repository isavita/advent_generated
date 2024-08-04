fs = require 'fs'

main = ->
  input = fs.readFileSync('input.txt', 'utf8').trim()
  console.log solve(input)

solve = (input) ->
  matrix = parseInput(input)
  originCol = matrix[0].indexOf('+')
  matrix[matrix.length - 1] = matrix[matrix.length - 1].map(() -> '#')

  ans = 0
  while not dropSand(matrix, originCol)
    ans++
    break if matrix[0][originCol] == 'o'
  
  ans

parseInput = (input) ->
  coordSets = []
  lowestCol = Infinity
  highestRow = 0

  input.split('\n').forEach (line) ->
    rawCoords = line.split(' -> ')
    coords = []
    rawCoords.forEach (rawCoord) ->
      [col, row] = rawCoord.split(',').map(Number)
      coords.push [col, row]
      lowestCol = Math.min(lowestCol, col)
      highestRow = Math.max(highestRow, row)
    coordSets.push coords

  ExtraLeftSpace = 200
  highestCol = 0

  coordSets.forEach (set) ->
    set.forEach (coord) ->
      coord[0] -= lowestCol - ExtraLeftSpace
      highestCol = Math.max(highestCol, coord[0])

  matrix = Array.from { length: highestRow + 3 }, -> Array(highestCol + ExtraLeftSpace * 2).fill('.')

  coordSets.forEach (set) ->
    for i in [1...set.length]
      cols = [set[i - 1][0], set[i][0]].sort()
      rows = [set[i - 1][1], set[i][1]].sort()

      if cols[0] == cols[1]
        for r in [rows[0]..rows[1]]
          matrix[r][cols[0]] = '#'
      else if rows[0] == rows[1]
        for c in [cols[0]..cols[1]]
          matrix[rows[0]][c] = '#'

  originCol = 500 - lowestCol + ExtraLeftSpace
  matrix[0][originCol] = '+'
  matrix

dropSand = (matrix, originCol) ->
  r = 0
  c = originCol

  while r < matrix.length - 1
    below = matrix[r + 1][c]
    diagonallyLeft = matrix[r + 1][c - 1]
    diagonallyRight = matrix[r + 1][c + 1]

    if below == '.'
      r++
    else if diagonallyLeft == '.'
      r++
      c--
    else if diagonallyRight == '.'
      r++
      c++
    else
      matrix[r][c] = 'o'
      return false

  true

main()