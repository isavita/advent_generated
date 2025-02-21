
fs = require 'fs'

# Reads the input file and returns the grid as a 2D array.
readFile = (filename) ->
  fs.readFileSync(filename, 'utf8').trim().split('\n').map (line) -> line.split('')

# Finds the starting position ('S') in the grid.
findStart = (grid) ->
  for row, y in grid
    for cell, x in row
      if cell is 'S'
        return { x, y }
  return null

# Determines the valid neighbors for a given cell based on its pipe type.
getNeighbors = (grid, x, y) ->
  cell = grid[y][x]
  neighbors = []
  #North
  if y > 0 and (cell in ['S', '|', 'L', 'J']) and (grid[y - 1][x] in ['|', '7', 'F'])
    neighbors.push { x: x, y: y - 1 }
  #South
  if y < grid.length - 1 and (cell in ['S', '|', '7', 'F']) and (grid[y + 1][x] in ['|', 'L', 'J'])
    neighbors.push { x: x, y: y + 1 }
  #West
  if x > 0 and (cell in ['S', '-', 'J', '7']) and (grid[y][x - 1] in ['-', 'L', 'F'])
    neighbors.push { x: x - 1, y: y }
  #East
  if x < grid[0].length - 1 and (cell in ['S', '-', 'L', 'F']) and (grid[y][x + 1] in ['-', 'J', '7'])
    neighbors.push { x: x + 1, y: y }
  neighbors

# Performs a breadth-first search to find the loop and distances.
bfs = (grid, start) ->
  queue = [{ x: start.x, y: start.y, dist: 0 }]
  visited = new Set()
  visited.add "#{start.x},#{start.y}"
  maxDist = 0
  loopCoords = new Set() # Store coordinates of loop tiles

  while queue.length > 0
    current = queue.shift()
    maxDist = Math.max(maxDist, current.dist)
    loopCoords.add "#{current.x},#{current.y}" # Add to loop coordinates


    for neighbor in getNeighbors(grid, current.x, current.y)
      neighborKey = "#{neighbor.x},#{neighbor.y}"
      if not visited.has neighborKey
        visited.add neighborKey
        queue.push { x: neighbor.x, y: neighbor.y, dist: current.dist + 1 }

  return { maxDist, loopCoords }


# Part 2:  Counts enclosed tiles using the even-odd rule (winding number).
countEnclosedTiles = (grid, loopCoords) ->
  enclosedCount = 0
  for y in [0...grid.length]
    inside = false
    for x in [0...grid[0].length]
      if loopCoords.has "#{x},#{y}"
        # Check for vertical crossing or corner pipes that change 'inside' state
        current = grid[y][x]
        if current in ['|', 'L', 'J']  # Crossing detected, toggle 'inside'
           inside = !inside
      else
        enclosedCount++ if inside

  enclosedCount


# --- Main Execution ---
grid = readFile 'input.txt'
start = findStart grid

# Part 1: Find the farthest distance.
{ maxDist, loopCoords } = bfs grid, start
console.log "Part 1: Farthest distance = #{maxDist}"

# Part 2: Count enclosed tiles
enclosedTiles = countEnclosedTiles(grid, loopCoords)
console.log "Part 2: Enclosed tiles = #{enclosedTiles}"
