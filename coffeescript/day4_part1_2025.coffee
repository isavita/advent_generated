
fs = require 'fs'

(() ->
  grid = fs.readFileSync('input.txt', 'utf8').trimEnd().split /\r?\n/
  rows = grid.length
  cols = grid[0].length
  dirs = [-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1]
  count = 0

  for i in [0...rows]
    row = grid[i]
    for j in [0...cols]
      continue if row[j] != '@'
      neighbors = 0
      for d in [0...8]
        ni = i + dirs[2*d]
        nj = j + dirs[2*d + 1]
        if ni >= 0 and ni < rows and nj >= 0 and nj < cols and grid[ni][nj] == '@'
          neighbors++
          break if neighbors >= 4
      count++ if neighbors < 4

  console.log count
)()
