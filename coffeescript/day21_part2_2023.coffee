
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim()
grid = input.split('\n').map (row) -> row.split('')
rows = grid.length
cols = grid[0].length

findStart = (grid) ->
  for r in [0...rows]
    for c in [0...cols]
      if grid[r][c] == 'S'
        return [r, c]

start = findStart(grid)

solvePart1 = (grid, steps) ->
  rows = grid.length
  cols = grid[0].length
  
  reachable = new Set()
  reachable.add(start.join(','))
  
  for _ in [0...steps]
    nextReachable = new Set()
    reachable.forEach (coord) ->
      [r, c] = coord.split(',').map(Number)
      
      neighbors = [
        [r - 1, c],
        [r + 1, c],
        [r, c - 1],
        [r, c + 1]
      ]
      
      neighbors.forEach ([nr, nc]) ->
        if nr >= 0 and nr < rows and nc >= 0 and nc < cols and grid[nr][nc] != '#'
          nextReachable.add([nr, nc].join(','))
    reachable = nextReachable
  
  return reachable.size

console.log "Part 1:", solvePart1(grid, 64)

solvePart2 = (grid, steps) ->
  rows = grid.length
  cols = grid[0].length
  
  start = findStart(grid)
  
  # Observation: the grid repeats infinitely.
  # Steps are large (26501365)
  # rows == cols and start is in the center. rows is odd
  # 26501365 = 65 + 202300 * 131
  
  # Count reachable tiles for a few values, then use quadratic interpolation
  
  distances = []
  
  reachable = new Set()
  reachable.add(start.join(','))
  
  stepsNeeded = [65, 65 + rows, 65 + 2 * rows]
  results = []

  maxSteps = Math.max(...stepsNeeded)

  for step in [1..maxSteps]
    nextReachable = new Set()
    reachable.forEach (coord) ->
      [r, c] = coord.split(',').map(Number)

      neighbors = [
        [r - 1, c],
        [r + 1, c],
        [r, c - 1],
        [r, c + 1]
      ]
      
      neighbors.forEach ([nr, nc]) ->
        # Wrap around the grid
        newR = (nr % rows + rows) % rows
        newC = (nc % cols + cols) % cols
        
        if grid[newR][newC] != '#'
          nextReachable.add([nr, nc].join(','))
    
    reachable = nextReachable

    if stepsNeeded.includes(step)
      results.push(reachable.size)

  # Quadratic interpolation
  y0 = results[0]
  y1 = results[1]
  y2 = results[2]

  n = Math.floor(steps / rows)
  
  a = (y2 - 2 * y1 + y0) / 2
  b = y1 - y0 - a
  c = y0

  result = a * n * n + b * n + c
  
  return result

console.log "Part 2:", solvePart2(grid, 26501365)
