fs = require 'fs'

totalRows = 400000 # Total number of rows to generate

countSafeTiles = (firstRow, totalRows) ->
  currentRow = firstRow
  safeCount = countChar currentRow, '.'

  for i in [1...totalRows]
    nextRow = ''
    for j in [0...currentRow.length]
      if isTrap j-1, j, j+1, currentRow
        nextRow += '^'
      else
        nextRow += '.'
        safeCount++

    currentRow = nextRow

  safeCount

isTrap = (left, center, right, row) ->
  l = safeIfOutOfBounds left, row
  c = row.charAt center
  r = safeIfOutOfBounds right, row

  (l == '^' && c == '^' && r == '.') ||
  (c == '^' && r == '^' && l == '.') ||
  (l == '^' && c == '.' && r == '.') ||
  (r == '^' && c == '.' && l == '.')

safeIfOutOfBounds = (index, row) ->
  return '.' if index < 0 || index >= row.length
  row.charAt index

countChar = (str, char) ->
  count = 0
  for c in str
    count++ if c == char
  count

readFirstRow = (filename) ->
  fs.readFileSync(filename, 'utf8').split("\n")[0]

firstRow = readFirstRow "input.txt"
safeTilesCount = countSafeTiles firstRow, totalRows
console.log safeTilesCount