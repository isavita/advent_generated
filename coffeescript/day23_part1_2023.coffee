
fs = require 'fs'

class Day23

  constructor: (@grid) ->
    @rows = @grid.length
    @cols = @grid[0].length
    @start = [0, @grid[0].indexOf('.')]
    @end = [@rows - 1, @grid[@rows - 1].indexOf('.')]

  solve: () ->
    @longestPath(@start, new Set())

  longestPath: (pos, visited) ->
    [row, col] = pos

    return 0 if row is @end[0] and col is @end[1] # Reached the end

    visited.add "#{row},#{col}"  #Mark as visited using string representation

    maxSteps = -Infinity  # Initialize with negative infinity to handle dead ends correctly

    # Define possible moves (including slopes)
    moves = [ [-1, 0, '^'], [1, 0, 'v'], [0, -1, '<'], [0, 1, '>'] ]


    for [dr, dc, slope] in moves
      newRow = row + dr
      newCol = col + dc
      newPos = [newRow,newCol]


      continue if newRow < 0 or newRow >= @rows or newCol < 0 or newCol >= @cols # Bounds check
      continue if @grid[newRow][newCol] is '#' # Forest
      continue if visited.has "#{newRow},#{newCol}" # Already visited
      continue if @grid[row][col] isnt '.' and @grid[row][col] isnt slope # Check for valid slope direction
      
      
      steps = @longestPath(newPos, new Set(visited)) # new Set to avoid side effects
      maxSteps = Math.max(maxSteps, steps)  if steps > -Infinity

    
    if maxSteps > -Infinity # if we find at least a way to the end, increment steps
      return maxSteps + 1
    else 
      return -Infinity




# --- Main Execution ---
try
  data = fs.readFileSync('input.txt', 'utf8').trim()
  grid = data.split('\n').map (line) -> line.split('')

  solver = new Day23(grid)
  result = solver.solve()
  console.log result

catch e
  console.error 'Error reading file or processing:', e
