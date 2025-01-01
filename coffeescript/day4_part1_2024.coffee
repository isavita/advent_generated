
fs = require 'fs'

getAllDirections = ->
  [
    [0, 1],   # right
    [1, 0],   # down
    [1, 1],   # diagonal down-right
    [-1, 1],  # diagonal up-right
    [0, -1],  # left
    [-1, 0],  # up
    [-1, -1], # diagonal up-left
    [1, -1]   # diagonal down-left
  ]

checkWord = (grid, word, x, y, dx, dy) ->
  return false if x < 0 or y < 0 or x >= grid.length or y >= grid[0].length
  for i in [0...word.length]
    newX = x + (dx * i)
    newY = y + (dy * i)
    return false if newX < 0 or newY < 0 or newX >= grid.length or newY >= grid[0].length
    return false if grid[newX][newY] != word[i]
  true

countOccurrences = (grid, word) ->
  count = 0
  directions = getAllDirections()
  for x in [0...grid.length]
    for y in [0...grid[x].length]
      for dir in directions
        if checkWord(grid, word, x, y, dir[0], dir[1])
          count++
  count

try
  grid = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').filter (line) -> line.trim() != ''
  count = countOccurrences(grid, 'XMAS')
  console.log "XMAS appears #{count} times in the word search"
catch error
  console.error "Error:", error
