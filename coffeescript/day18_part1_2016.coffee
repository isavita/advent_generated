fs = require 'fs'

totalRows = 40

readFirstRow = (filename) ->
  fileContent = fs.readFileSync filename, 'utf8'
  fileContent.split("\n")[0]

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
  c = row[center]
  r = safeIfOutOfBounds right, row

  (l == '^' and c == '^' and r == '.') or
  (c == '^' and r == '^' and l == '.') or
  (l == '^' and c == '.' and r == '.') or
  (r == '^' and c == '.' and l == '.')

safeIfOutOfBounds = (index, row) ->
  if index < 0 or index >= row.length
    '.'
  else
    row[index]

countChar = (str, char) ->
  count = 0
  for c in str
    count++ if c == char
  count

firstRow = readFirstRow "input.txt"
safeTilesCount = countSafeTiles firstRow, totalRows
console.log safeTilesCount