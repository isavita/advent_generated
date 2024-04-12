fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  lines = data.trim().split '\n'
  slopes = [
    [1, 1]
    [3, 1]
    [5, 1]
    [7, 1]
    [1, 2]
  ]

  product = 1

  for slope in slopes
    treeCount = 0
    pos = 0
    i = 0
    while i < lines.length
      treeCount++ if lines[i][pos] == '#'
      pos = (pos + slope[0]) % lines[i].length
      i += slope[1]

    product *= treeCount

  console.log product