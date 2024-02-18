fs = require 'fs'

# Function to calculate the position in the sequence
calculatePosition = (row, column) ->
  # Calculate the diagonal position
  n = row + column - 1
  # Calculate the sequence position using the sum of integers formula
  sequencePosition = n * (n - 1) / 2 + column
  sequencePosition

# Function to generate the code for a given position
generateCode = (position) ->
  code = 20151125
  multiplier = 252533
  modulus = 33554393
  for i in [2..position]
    code = (code * multiplier) % modulus
  code

# Function to extract row and column from the input file
extractRowAndColumn = (text) ->
  /row (\d+), column (\d+)/.exec(text).slice(1, 3).map(Number)

# Read the input file and solve the puzzle
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return
  [targetRow, targetColumn] = extractRowAndColumn(data)
  position = calculatePosition(targetRow, targetColumn)
  code = generateCode(position)
  console.log "The code for position (#{targetRow}, #{targetColumn}) is: #{code}"
