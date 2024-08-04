fs = require 'fs'

readFileToMatrix = (filePath) ->
  matrix = []
  try
    data = fs.readFileSync(filePath, 'utf8')
    for line in data.split('\n')
      matrix.push line.split('')
  catch err
    console.error "Error reading file:", err
  matrix

sumOfPartNumbers = (matrix) ->
  sum = 0
  visited = (new Array(matrix.length)).fill(0).map -> new Array(matrix[0].length).fill(false)

  for y in [0...matrix.length]
    for x in [0...matrix[y].length]
      if !visited[y][x] && /\d/.test(matrix[y][x])
        [number, length] = extractNumber(matrix, x, y)
        if isAdjacentToSymbol(matrix, x, y, length)
          sum += number
        for i in [0...length]
          visited[y][x+i] = true
  sum

extractNumber = (matrix, x, y) ->
  numberStr = ''
  while x < matrix[y].length && /\d/.test(matrix[y][x])
    numberStr += matrix[y][x]
    x++
  [parseInt(numberStr), numberStr.length]

isAdjacentToSymbol = (matrix, x, y, length) ->
  for i in [0...length]
    if checkAdjacent(matrix, x+i, y)
      return true
  false

checkAdjacent = (matrix, x, y) ->
  for dy in [-1..1]
    for dx in [-1..1]
      adjX = x + dx
      adjY = y + dy
      if adjY >= 0 && adjY < matrix.length && adjX >= 0 && adjX < matrix[adjY].length
        if !/\d/.test(matrix[adjY][adjX]) && matrix[adjY][adjX] != '.'
          return true
  false

matrix = readFileToMatrix('input.txt')
sum = sumOfPartNumbers(matrix)
console.log sum