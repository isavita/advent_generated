fs = require 'fs'

rules = {}

fs.readFile 'input.txt', 'utf8', (err, data) ->
  for line in data.split('\n')
    [from, to] = line.split(' => ')
    rules[from] = to

  grid = [
    ".#."
    "..#"
    "###"
  ]

  enhance = (input, rules) ->
    for i in [0..3]
      return rules[input] if rules[input]?
      input = rotate input
    input = flip input
    for i in [0..3]
      return rules[input] if rules[input]?
      input = rotate input
    ''

  rotate = (input) ->
    parts = input.split('/')
    size = parts.length
    newParts = []
    for x in [0...size]
      newRow = ''
      for y in [size-1..0]
        newRow += parts[y][x]
      newParts.push newRow
    newParts.join '/'

  flip = (input) ->
    parts = input.split('/')
    for part, i in parts
      parts[i] = reverse part
    parts.join '/'

  reverse = (input) ->
    input.split('').reverse().join('')

  for i in [0..4]
    if grid.length % 2 == 0
      subSize = 2
      newSize = grid.length / 2 * 3
    else
      subSize = 3
      newSize = grid.length / 3 * 4

    newGrid = []
    for x in [0...newSize]
      newGrid.push ''

    for y in [0...grid.length] by subSize
      for x in [0...grid.length] by subSize
        square = []
        for dy in [0...subSize]
          square.push grid[y + dy][x...x + subSize]
        newSquare = enhance(square.join('/'), rules)
        for row, dy in newSquare.split('/')
          newGrid[Math.floor(y / subSize) * (subSize + 1) + dy] += row

    grid = newGrid

  count = 0
  for row in grid
    for pixel in row
      count++ if pixel == '#'

  console.log count