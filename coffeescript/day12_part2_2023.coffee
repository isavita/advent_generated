fs = require 'fs'

class Row
  constructor: (@springs, @group) ->

parseInput = (input) ->
  rows = []
  for line in input
    parts = line.split ' '
    springs = parts[0]
    ints = parseStringToInts parts[1]
    row = new Row springs, ints
    rows.push row
  rows

parseStringToInts = (numbersLine) ->
  numbers = []
  numbersParts = numbersLine.split ','
  for numberStr in numbersParts
    number = parseInt numberStr
    throw new Error 'Invalid number' unless number?
    numbers.push number
  numbers

countArrangementsRecursive = (row, iSprings, iGroup, iContiguousDamaged, cache) ->
  if iSprings == row.springs.length
    if iGroup == row.group.length and iContiguousDamaged == 0
      return 1
    else if iGroup == row.group.length - 1 and iContiguousDamaged == row.group[iGroup]
      return 1
    return 0

  cacheKey = "#{iSprings},#{iGroup},#{iContiguousDamaged}"
  if cache[cacheKey]?
    return cache[cacheKey]

  res = 0
  char = row.springs[iSprings]
  if char == '.' or char == '?'
    if iContiguousDamaged == 0
      res += countArrangementsRecursive row, iSprings + 1, iGroup, iContiguousDamaged, cache
    if iContiguousDamaged == row.group[iGroup]
      res += countArrangementsRecursive row, iSprings + 1, iGroup + 1, 0, cache
  if char == '#' or char == '?'
    if iGroup < row.group.length and iContiguousDamaged < row.group[iGroup]
      res += countArrangementsRecursive row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache

  cache[cacheKey] = res
  res

countArrangements = (row) ->
  countArrangementsRecursive row, 0, 0, 0, {}

unfoldRow = (row, unfoldingFactor) ->
  newRow = new Row row.springs, row.group.slice()
  for i in [1...unfoldingFactor]
    newRow.springs += '?' + row.springs
    newRow.group.push row.group...
  newRow

solve = (input) ->
  rows = parseInput input
  unfoldedRows = []
  for row in rows
    unfoldedRows.push unfoldRow row, 5
  res = 0
  for row in unfoldedRows
    res += countArrangements row
  res

readFile = (fileName) ->
  file = fs.readFileSync fileName, 'utf8'
  file.trim().split '\n'

main = ->
  input = readFile 'input.txt'
  console.log solve input

main()