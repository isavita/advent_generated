fs = require 'fs'

# Read input from file
input = fs.readFileSync 'input.txt', 'utf8'

# Split input into rows
rows = input.split '\n'

# Initialize variables
visibleTrees = 0
maxRow = rows.length
maxCol = rows[0].length

# Count visible trees on edges
visibleTrees += 2 * (maxRow + maxCol - 2)

# Count visible trees in interior
for row in [1...maxRow-1]
  for col in [1...maxCol-1]
    treeHeight = parseInt rows[row][col]
    left = (parseInt rows[row][c] for c in [0...col]).every (x) -> x < treeHeight
    right = (parseInt rows[row][c] for c in [col+1...maxCol]).every (x) -> x < treeHeight
    up = (parseInt rows[r][col] for r in [0...row]).every (x) -> x < treeHeight
    down = (parseInt rows[r][col] for r in [row+1...maxRow]).every (x) -> x < treeHeight
    if left or right or up or down
      visibleTrees++

console.log visibleTrees