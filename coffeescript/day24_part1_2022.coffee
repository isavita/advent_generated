
fs = require 'fs'

# Function to parse the input and return the grid, blizzard positions, start, and end.
parseInput = (data) ->
  lines = data.trim().split('\n')
  grid = lines.map (line) -> line.split('')
  
  rows = grid.length
  cols = grid[0].length
  
  start = [0, grid[0].indexOf('.')]
  end = [rows - 1, grid[rows - 1].indexOf('.')]

  blizzards = []
  for r in [0...rows]
    for c in [0...cols]
      if grid[r][c] in ['>', '<', '^', 'v']
        blizzards.push([r, c, grid[r][c]])

  return {grid, blizzards, start, end, rows, cols}

# Function to calculate the blizzard positions at a given time.
calculateBlizzardPositions = (blizzards, time, rows, cols) ->
  newBlizzards = {}
  for [r, c, dir] in blizzards
    newR = r
    newC = c
    switch dir
      when '>'
        newC = (c - 1 + time) % (cols - 2) + 1
      when '<'
        newC = (c - 1 - time) % (cols - 2)
        newC = if newC < 0 then newC + cols - 2 else newC
        newC += 1
      when 'v'
        newR = (r - 1 + time) % (rows - 2) + 1
      when '^'
        newR = (r - 1 - time) % (rows - 2)
        newR = if newR < 0 then newR + rows - 2 else newR
        newR += 1

    newBlizzards["#{newR},#{newC}"] = true

  return newBlizzards

# Function to find the shortest path using BFS.
findShortestPath = (grid, blizzards, start, end, rows, cols) ->
  queue = [[start[0], start[1], 0]]  # [row, col, time]
  visited = {}
  visited["#{start[0]},#{start[1]},0"] = true

  while queue.length > 0
    [r, c, time] = queue.shift()

    if r == end[0] and c == end[1]
      return time
    
    nextTime = time + 1
    nextBlizzards = calculateBlizzardPositions(blizzards, nextTime, rows, cols)
    
    # Possible moves: up, down, left, right, wait
    moves = [[0, 1], [0, -1], [1, 0], [-1, 0], [0, 0]]

    for [dr, dc] in moves
      nr = r + dr
      nc = c + dc

      if (0 <= nr < rows) and (0 <= nc < cols) and grid[nr][nc] != '#' and !nextBlizzards["#{nr},#{nc}"]
        key = "#{nr},#{nc},#{nextTime}"
        if !visited[key]
          visited[key] = true
          queue.push([nr, nc, nextTime])

  return -1 # Should not happen given puzzle constraints

# Main function to read input and solve the problem.
solve = ->
  data = fs.readFileSync('input.txt', 'utf8')
  {grid, blizzards, start, end, rows, cols} = parseInput(data)

  shortestTime = findShortestPath(grid, blizzards, start, end, rows, cols)
  console.log shortestTime

solve()
