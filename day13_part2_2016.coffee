fs = require 'fs'

# Function to check if a position is an open space or a wall
isOpenSpace = (x, y, favoriteNumber) ->
  value = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
  binary = value.toString(2)
  ones = binary.split('').filter((bit) -> bit == '1').length
  ones % 2 == 0

# Modified BFS to find how many unique locations can be reached within 50 steps
findReachableLocations = (favoriteNumber, maxSteps) ->
  queue = [[1, 1, 0]] # Array of [x, y, steps]
  visited = {"1,1": true} # Mark the starting location as visited
  while queue.length > 0
    [x, y, steps] = queue.shift()
    # Stop adding new positions if we've reached 50 steps, but continue processing existing ones in the queue
    if steps < maxSteps
      # Directions: right, left, up, down
      [[x+1, y], [x-1, y], [x, y+1], [x, y-1]].forEach (nextPosition) ->
        [nextX, nextY] = nextPosition
        if nextX >= 0 and nextY >= 0 and isOpenSpace(nextX, nextY, favoriteNumber) and not visited[[nextX, nextY]]
          visited[[nextX, nextY]] = true
          queue.push([nextX, nextY, steps + 1])
  # Return the count of unique locations visited
  Object.keys(visited).length

# Read the favorite number from the input file and solve the puzzle
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return
  favoriteNumber = parseInt data.trim()
  locations = findReachableLocations(favoriteNumber, 50)
  console.log locations

