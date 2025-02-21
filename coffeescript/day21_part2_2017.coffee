
fs = require 'fs'

# Function to rotate a grid 90 degrees clockwise
rotate = (grid) ->
  gridSize = grid.length
  newGrid = []
  for i in [0...gridSize]
    newRow = []
    for j in [0...gridSize]
      newRow.push grid[gridSize - 1 - j][i]
    newGrid.push newRow
  newGrid

# Function to flip a grid horizontally
flip = (grid) ->
  grid.slice().reverse()

# Function to convert a grid to its string representation
gridToString = (grid) ->
  grid.map((row) -> row.join('')).join('/')

# Function to convert a string representation to a grid
stringToGrid = (str) ->
  str.split('/').map (row) -> row.split('')
  
# Function to generate all variations (rotations and flips) of a grid
generateVariations = (grid) ->
  variations = []
  currentGrid = grid
  for i in [0...4]
    variations.push gridToString(currentGrid)
    variations.push gridToString(flip(currentGrid))
    currentGrid = rotate(currentGrid)
  variations

# Function to enhance the grid based on the rules
enhance = (grid, rules) ->
  size = grid.length
  if size % 2 == 0
    newSize = size / 2 * 3
    blockSize = 2
    newBlockSize = 3
  else
    newSize = size / 3 * 4
    blockSize = 3
    newBlockSize = 4
    
  newGrid = []
  for i in [0...newSize]
      newGrid.push [] # initialize new rows
  
  for r in [0...size/blockSize]
    for c in [0...size/blockSize]
        block = []
        for i in [0...blockSize]
            row = []
            for j in [0...blockSize]
                row.push grid[r*blockSize + i][c*blockSize + j]
            block.push row

        replacement = rules[gridToString(block)]
        replacementGrid = stringToGrid(replacement)

        for i in [0...newBlockSize]
            for j in [0...newBlockSize]
                newGrid[r * newBlockSize + i][c * newBlockSize + j] = replacementGrid[i][j]
  newGrid

# Read input and parse rules
input = fs.readFileSync('input.txt', 'utf8').trim()
rules = {}
input.split('\n').forEach (line) ->
  [pattern, result] = line.split(' => ')
  generateVariations(stringToGrid(pattern)).forEach (variation) ->
    rules[variation] = result

# Initial grid
grid = [
  ['.', '#', '.'],
  ['.', '.', '#'],
  ['#', '#', '#']
]

# Perform iterations
iterations1 = 5
iterations2 = 18

for i in [1..iterations1]
    grid = enhance(grid, rules)

countOnPixels = (grid) ->
    count = 0
    for row in grid
        for pixel in row
            if pixel == '#'
                count++
    count

console.log "Part 1: #{countOnPixels(grid)}"

for i in [iterations1+1..iterations2]
    grid = enhance(grid, rules)

console.log "Part 2: #{countOnPixels(grid)}"
