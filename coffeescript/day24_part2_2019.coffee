
fs = require 'fs'

# --- Helper Functions ---

# Calculate biodiversity rating
calculateBiodiversity = (grid) ->
  biodiversity = 0
  for row, y in grid
    for cell, x in row
      if cell is '#'
        biodiversity += Math.pow(2, (y * 5 + x))
  biodiversity

# Convert grid to a string representation for easy comparison
gridToString = (grid) ->
    grid.map((row) -> row.join('')).join('\n')

# Count adjacent bugs
countAdjacentBugs = (grid, x, y) ->
  count = 0
  count += 1 if x > 0 and grid[y][x-1] is '#'
  count += 1 if x < 4 and grid[y][x+1] is '#'
  count += 1 if y > 0 and grid[y-1][x] is '#'
  count += 1 if y < 4 and grid[y+1][x] is '#'
  count

# Simulate one minute of bug life/death (Part 1)
simulateStep = (grid) ->
  newGrid = []
  for y in [0..4]
    newRow = []
    for x in [0..4]
      adjacentBugs = countAdjacentBugs(grid, x, y)
      if grid[y][x] is '#'
        newRow.push(if adjacentBugs is 1 then '#' else '.')
      else
        newRow.push(if adjacentBugs is 1 or adjacentBugs is 2 then '#' else '.')
    newGrid.push newRow
  newGrid


# --- Part 1 ---
solvePart1 = (initialGrid) ->
    seenGrids = new Set()
    currentGrid = initialGrid
    
    while true
        gridStr = gridToString(currentGrid)
        if seenGrids.has(gridStr)
            return calculateBiodiversity(currentGrid)
        seenGrids.add(gridStr)
        currentGrid = simulateStep(currentGrid)
    

# --- Part 2 ---

# Count adjacent bugs in recursive grids
countRecursiveAdjacentBugs = (levels, level, x, y) ->
  count = 0

  # Adjacent cells within the same level
  count += 1 if x > 0 and levels[level]?[y]?[x-1] is '#'
  count += 1 if x < 4 and levels[level]?[y]?[x+1] is '#'
  count += 1 if y > 0 and levels[level]?[y-1]?[x] is '#'
  count += 1 if y < 4 and levels[level]?[y+1]?[x] is '#'

  # Handle edges and the center tile (recursive levels)
  # Up
  if y is 0
    count += 1 if levels[level - 1]?[1]?[2] is '#'  # Outer level, row 1, col 2
  else if y is 3 and x is 2
    for i in [0..4]
      count += 1 if levels[level + 1]?[4]?[i] is '#' # Inner Level, row 4
  
  # Down
  if y is 4
    count += 1 if levels[level - 1]?[3]?[2] is '#'  # Outer level, row 3, col 2
  else if y is 1 and x is 2
    for i in [0..4]
      count += 1 if levels[level + 1]?[0]?[i] is '#'   # Inner level, row 0

  # Left
  if x is 0
    count += 1 if levels[level - 1]?[2]?[1] is '#' # Outer Level, row 2, col 1
  else if y is 2 and x is 3
      for i in [0..4]
          count += 1 if levels[level + 1]?[i]?[4] is '#' # Inner Level, col 4
  
  # Right
  if x is 4
      count +=1 if levels[level - 1]?[2]?[3] is '#' # Outer Level, row 2, col 3
  else if y is 2 and x is 1
      for i in [0..4]
        count += 1 if levels[level+1]?[i]?[0] is '#' # Inner Level, col 0
        
  count


# Simulate one step of bug life/death in recursive grids.
simulateRecursiveStep = (levels) ->
  newLevels = {}

  # Determine the range of levels to consider (expand if necessary)
  minLevel = Math.min(0, ...Object.keys(levels).map(Number)) - 1
  maxLevel = Math.max(0, ...Object.keys(levels).map(Number)) + 1

  for level in [minLevel..maxLevel]
    newGrid = []
    for y in [0..4]
      newRow = []
      for x in [0..4]
        if x is 2 and y is 2
            newRow.push('.') # Center tile is always empty
            continue
        
        adjacentBugs = countRecursiveAdjacentBugs(levels, level, x, y)

        if levels[level]?[y]?[x] is '#'
          newRow.push(if adjacentBugs is 1 then '#' else '.')
        else
          newRow.push(if adjacentBugs is 1 or adjacentBugs is 2 then '#' else '.')
      newGrid.push(newRow)
    newLevels[level] = newGrid
  newLevels



solvePart2 = (initialGrid) ->
    levels = { 0: initialGrid }
    for i in [1..200]
        levels = simulateRecursiveStep(levels)

    # Count total bugs
    totalBugs = 0
    for level of levels
        for row in levels[level]
            for cell in row
                totalBugs += 1 if cell is '#'

    totalBugs

# --- Main Execution ---

# Read input from file
inputFile = 'input.txt'
inputData = fs.readFileSync(inputFile, 'utf8').trim().split('\n')
initialGrid = inputData.map (line) -> line.split('')


# Solve Part 1
part1Result = solvePart1(initialGrid)
console.log "Part 1: #{part1Result}"

# Solve Part 2
part2Result = solvePart2(initialGrid)
console.log "Part 2: #{part2Result}"
