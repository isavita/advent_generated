
fs = require 'fs'

# Define the shapes of the rocks
shapes = [
  [[0, 0], [1, 0], [2, 0], [3, 0]],     # Horizontal line
  [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]], # Plus sign
  [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]], # Reverse L
  [[0, 0], [0, 1], [0, 2], [0, 3]],     # Vertical line
  [[0, 0], [1, 0], [0, 1], [1, 1]]      # Square
]

# Function to check if a rock can be placed at a given position
canPlace = (rock, chamber, x, y) ->
  for coord in rock
    nx = x + coord[0]
    ny = y + coord[1]
    if nx < 0 or nx >= 7 or ny < 0 or chamber[ny]?[nx]
      return false
  return true

# Function to place a rock in the chamber
placeRock = (rock, chamber, x, y) ->
  for coord in rock
    nx = x + coord[0]
    ny = y + coord[1]
    chamber[ny] ||= []
    chamber[ny][nx] = true

# Function to simulate the falling rocks
simulateRocks = (jetPattern, numRocks) ->
  chamber = []
  jetIndex = 0
  height = 0

  for rockIndex in [0...numRocks]
    rock = shapes[rockIndex % shapes.length]
    x = 2
    y = height + 3

    while true
      # Push the rock with the jet
      direction = if jetPattern[jetIndex] is '>' then 1 else -1
      jetIndex = (jetIndex + 1) % jetPattern.length
      if canPlace(rock, chamber, x + direction, y)
        x += direction

      # Try to move the rock down
      if canPlace(rock, chamber, x, y - 1)
        y -= 1
      else
        # Rock comes to rest
        placeRock(rock, chamber, x, y)
        height = Math.max(height, y + rock.reduce(((max, c) -> Math.max(max, c[1] + 1)), 0))
        break

  return height

# Read the jet pattern from the input file
jetPattern = fs.readFileSync('input.txt', 'utf8').trim()

# Simulate 2022 rocks and get the tower height
towerHeight = simulateRocks(jetPattern, 2022)

# Print the tower height
console.log towerHeight
