
fs = require 'fs'

# Read input from file
[boardMap, path] = fs.readFileSync('input.txt', 'utf8').split('\n\n')

# Parse the board map into a 2D array, handling variable row lengths
board = boardMap.split('\n').map (row) ->
  row.split('')

# Parse the path into a sequence of movements and turns
moves = path.trim().split(/([RL])/).filter (move) -> move.length > 0

# Determine board dimensions
maxRow = board.length
maxCol = Math.max.apply(null, (row.length for row in board))

# Function to find the opposite edge of the board
findOppositeEdge = (row, col, direction) ->
  switch direction
    when 0 # Right
      c = 0
      while board[row]?[c] is ' ' or board[row]?[c] is undefined
        c++
      return [row, c]
    when 1 # Down
      r = 0
      while board[r]?[col] is ' ' or board[r]?[col] is undefined
        r++
      return [r, col]
    when 2 # Left
      c = board[row].length - 1
      while board[row]?[c] is ' ' or board[row]?[c] is undefined
        c--
      return [row, c]
    when 3 # Up
      r = board.length - 1
      while board[r]?[col] is ' ' or board[r]?[col] is undefined
        r--
      return [r, col]

# Function to move one step, handling wrapping
moveOneStep = (row, col, direction) ->
  switch direction
    when 0 # Right
      newCol = (col + 1) % board[row].length
      if board[row][newCol] is ' ' or board[row][newCol] is undefined
          [row, newCol] = findOppositeEdge(row, col, direction)
      return [row, newCol]
    when 1 # Down
      newRow = (row + 1) % board.length
      if board[newRow]?[col] is ' ' or board[newRow]?[col] is undefined
        [newRow, col] = findOppositeEdge(row, col, direction)
      return [newRow, col]
    when 2 # Left
      newCol = (col - 1 + board[row].length) % board[row].length
      if board[row][newCol] is ' ' or board[row][newCol] is undefined
        [row, newCol] = findOppositeEdge(row, col, direction)
      return [row, newCol]
    when 3 # Up
      newRow = (row - 1 + board.length) % board.length
      if board[newRow]?[col] is ' ' or board[newRow]?[col] is undefined
        [newRow, col] = findOppositeEdge(row, col, direction)
      return [newRow, col]


# Starting position (leftmost open tile in the top row)
startRow = 0
startCol = board[0].indexOf('.')

# Initial facing (right)
facing = 0

# Current position
currentRow = startRow
currentCol = startCol

# Process each move in the path
for move in moves
  if move is 'R'
    facing = (facing + 1) % 4
  else if move is 'L'
    facing = (facing - 1 + 4) % 4
  else
    steps = parseInt(move)
    for i in [0...steps]
      [nextRow, nextCol] = moveOneStep(currentRow, currentCol, facing)
      if board[nextRow][nextCol] is '#'
        break  # Stop if we hit a wall
      else
        currentRow = nextRow
        currentCol = nextCol

# Calculate final password
finalPassword = 1000 * (currentRow + 1) + 4 * (currentCol + 1) + facing
console.log finalPassword

