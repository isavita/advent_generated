
fs = require 'fs'

checkMAS = (grid, x, y, dx, dy) ->
  return false if x < 0 or y < 0 or x >= grid.length or y >= grid[0].length
  word = 'MAS'
  forward = true
  backward = true
  
  for i in [0...word.length]
    newX = x + (dx * i)
    newY = y + (dy * i)
    if newX < 0 or newY < 0 or newX >= grid.length or newY >= grid[0].length
      forward = false
      break
    if grid[newX][newY] != word[i]
      forward = false
  
  for i in [0...word.length]
    newX = x + (dx * i)
    newY = y + (dy * i)
    if newX < 0 or newY < 0 or newX >= grid.length or newY >= grid[0].length
      backward = false
      break
    if grid[newX][newY] != word[word.length - 1 - i]
      backward = false
  
  forward or backward

checkXMAS = (grid, x, y) ->
  checkMAS(grid, x - 1, y - 1, 1, 1) and checkMAS(grid, x - 1, y + 1, 1, -1) or
  checkMAS(grid, x + 1, y - 1, -1, 1) and checkMAS(grid, x + 1, y + 1, -1, -1)

countXMASPatterns = (grid) ->
  count = 0
  return 0 if grid.length < 3 or grid[0].length < 3
  
  for i in [1...grid.length - 1]
    for j in [1...grid[i].length - 1]
      if grid[i][j] == 'A' and checkXMAS(grid, i, j)
        count++
  count

try
  grid = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').filter (line) -> line.trim() != ''
  count = countXMASPatterns(grid)
  console.log "X-MAS patterns appear #{count} times in the word search"
catch error
  console.error "Error:", error
