
fs = require 'fs'

# Function to find the starting position 'S' in the grid
findStart = (grid) ->
  for i in [0...grid.length]
    for j in [0...grid[i].length]
      if grid[i][j] is 'S'
        return [i, j]

# Function to determine valid neighbors based on pipe connections
getNeighbors = (grid, row, col) ->
  neighbors = []
  pipe = grid[row][col]

  # Define possible moves and pipe connections.
  moves = {
    '|': [[-1, 0], [1, 0]],  # North, South
    '-': [[0, -1], [0, 1]],  # West,  East
    'L': [[-1, 0], [0, 1]],  # North, East
    'J': [[-1, 0], [0, -1]],  # North, West
    '7': [[1, 0], [0, -1]],  # South, West
    'F': [[1, 0], [0, 1]],  # South, East
    'S': [[-1, 0], [1, 0], [0, -1], [0, 1]]  # All directions (for starting point)
  }
  
  validConnections = {
      '[-1, 0]': ['|', '7', 'F'], # North Connections
      '[1, 0]': ['|', 'L', 'J'],  # South Connections
      '[0, -1]': ['-', 'L', 'F'],  # West Connections
      '[0, 1]': ['-', '7', 'J']   # East Connections
  }

  if moves[pipe]?
    for move in moves[pipe]
      newRow = row + move[0]
      newCol = col + move[1]

      # Check bounds
      if newRow >= 0 and newRow < grid.length and newCol >= 0 and newCol < grid[newRow].length
        neighbor = grid[newRow][newCol]
        # For 'S', check any valid neighbor
        if (pipe is 'S' and validConnections["[#{move[0]}, #{move[1]}]"].includes neighbor)
            neighbors.push [newRow, newCol]
        #For other pipes
        else if (neighbor isnt '.' and validConnections["[#{move[0]}, #{move[1]}]"].includes neighbor)
            neighbors.push [newRow, newCol]

  neighbors

# Main function to calculate the farthest distance in the loop
solve = (grid) ->
  [startRow, startCol] = findStart grid
  queue = [[startRow, startCol, 0]]  # [row, col, distance]
  visited = new Set()
  visited.add "#{startRow},#{startCol}"
  maxDistance = 0

  while queue.length > 0
    [row, col, distance] = queue.shift()
    maxDistance = Math.max maxDistance, distance

    neighbors = getNeighbors grid, row, col
    for [nRow, nCol] in neighbors
      if not visited.has "#{nRow},#{nCol}"
        visited.add "#{nRow},#{nCol}"
        queue.push [nRow, nCol, distance + 1]

  maxDistance

# Read the input from the file
input = fs.readFileSync('input.txt', 'utf-8').trim()
grid = input.split('\n').map (line) -> line.split('')

# Calculate and print the result
result = solve grid
console.log result
