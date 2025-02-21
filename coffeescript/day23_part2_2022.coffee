
fs = require 'fs'

# Read input from file
input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

# Parse initial elf positions
elves = {}
for y in [0...input.length]
  for x in [0...input[y].length]
    if input[y][x] == '#'
      elves["#{x},#{y}"] = [x, y]

# Define directions and their checks
directions = [
  { name: 'N',  dx: 0, dy: -1, checks: [[0, -1], [1, -1], [-1, -1]] }
  { name: 'S',  dx: 0, dy: 1,  checks: [[0, 1], [1, 1], [-1, 1]] }
  { name: 'W',  dx: -1, dy: 0, checks: [[-1, 0], [-1, -1], [-1, 1]] }
  { name: 'E',  dx: 1, dy: 0,  checks: [[1, 0], [1, -1], [1, 1]] }
]

# Function to check if a position is occupied
isOccupied = (x, y, elves) ->
  elves["#{x},#{y}"]?

# Function to simulate one round
simulateRound = (elves, directions) ->
  proposals = {}
  proposedCounts = {}
  moved = false

  # First half: Elves propose moves
  for elfKey, [x, y] of elves
    # Check if any adjacent positions are occupied
    adjacentOccupied = false
    for dx in [-1..1]
      for dy in [-1..1]
        continue if dx == 0 and dy == 0
        if isOccupied(x + dx, y + dy, elves)
          adjacentOccupied = true
          break
      break if adjacentOccupied

    if not adjacentOccupied
      continue  # No need to move

    # Consider directions in order
    for dir in directions
      canMove = true
      for [checkDx, checkDy] in dir.checks
        if isOccupied(x + checkDx, y + checkDy, elves)
          canMove = false
          break
      if canMove
        proposedX = x + dir.dx
        proposedY = y + dir.dy
        proposals[elfKey] = [proposedX, proposedY]
        proposedKey = "#{proposedX},#{proposedY}"
        proposedCounts[proposedKey] = (proposedCounts[proposedKey] or 0) + 1
        break

  # Second half: Elves move if their proposal is unique
  for elfKey, [proposedX, proposedY] of proposals
    proposedKey = "#{proposedX},#{proposedY}"
    if proposedCounts[proposedKey] == 1
      delete elves[elfKey]
      elves[proposedKey] = [proposedX, proposedY]
      moved = true

  # Rotate directions
  firstDir = directions.shift()
  directions.push(firstDir)
  
  return moved

# Part 1: Simulate 10 rounds
elves1 = JSON.parse(JSON.stringify(elves))  # Deep copy for part 1
directions1 = directions.slice()  # Copy the array
for round in [1..10]
  simulateRound(elves1, directions1)

# Calculate bounding rectangle and empty tiles
minX = Infinity
minY = Infinity
maxX = -Infinity
maxY = -Infinity
for elfKey, [x, y] of elves1
  minX = Math.min(minX, x)
  minY = Math.min(minY, y)
  maxX = Math.max(maxX, x)
  maxY = Math.max(maxY, y)

emptyTiles = (maxX - minX + 1) * (maxY - minY + 1) - Object.keys(elves1).length
console.log "Part 1: #{emptyTiles}"

# Part 2: Find the first round with no moves
elves2 = JSON.parse(JSON.stringify(elves)) # Deep copy for part 2
directions2 = directions.slice() # Copy the array
round = 1
while simulateRound(elves2, directions2)
  round++

console.log "Part 2: #{round}"
