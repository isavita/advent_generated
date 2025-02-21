
fs = require 'fs'

# Function to solve the puzzle
solve = (map, part2 = false) ->
  rows = map.length
  cols = map[0].length
  start = [0, map[0].indexOf('.')]
  end = [rows - 1, map[rows - 1].indexOf('.')]
  
  # Directions: Up, Right, Down, Left
  dr = [-1, 0, 1, 0]
  dc = [0, 1, 0, -1]

  # Recursive Depth-First Search function
  dfs = (row, col, visited, steps) ->
    # Base case: Reached the end
    if row == end[0] and col == end[1]
      return steps

    maxSteps = -1
    
    for dir in [0..3]  # Iterate through each direction
      newRow = row + dr[dir]
      newCol = col + dc[dir]

      # Check for out-of-bounds and walls
      continue if newRow < 0 or newRow >= rows or newCol < 0 or newCol >= cols or map[newRow][newCol] == '#'

      # Check if the cell has been visited
      continue if visited.has "#{newRow},#{newCol}"

      # Slope handling for Part 1
      if not part2
        if map[row][col] == '^' and dir != 0 then continue
        if map[row][col] == '>' and dir != 1 then continue
        if map[row][col] == 'v' and dir != 2 then continue
        if map[row][col] == '<' and dir != 3 then continue
            
      # Mark the current cell as visited
      visited.add "#{newRow},#{newCol}"

      # Recursive call and update maximum steps
      maxSteps = Math.max(maxSteps, dfs(newRow, newCol, visited, steps + 1))

      # Backtrack: Remove the current cell from visited (for other paths)
      visited.delete "#{newRow},#{newCol}"
    
    return maxSteps

  # Start the DFS from the starting position
  initialVisited = new Set()
  initialVisited.add "#{start[0]},#{start[1]}"
  return dfs(start[0], start[1], initialVisited, 0)
  
#optimized solution using graph representation
solveOptimized = (map, part2 = false) ->
  rows = map.length
  cols = map[0].length
  start = [0, map[0].indexOf('.')]
  end = [rows - 1, map[rows - 1].indexOf('.')]

  # Directions: Up, Right, Down, Left
  dr = [-1, 0, 1, 0]
  dc = [0, 1, 0, -1]

  #build graph
  graph = {}
  buildGraph = (row, col) ->

    
    for dir in [0..3]  # Iterate through each direction
        newRow = row + dr[dir]
        newCol = col + dc[dir]

        # Check for out-of-bounds and walls
        continue if newRow < 0 or newRow >= rows or newCol < 0 or newCol >= cols or map[newRow][newCol] == '#'

        # Slope handling for Part 1
        if not part2
          if map[row][col] == '^' and dir != 0 then continue
          if map[row][col] == '>' and dir != 1 then continue
          if map[row][col] == 'v' and dir != 2 then continue
          if map[row][col] == '<' and dir != 3 then continue

        graph["#{row},#{col}"] ?= []
        graph["#{row},#{col}"].push([newRow, newCol])

  
  for r in [0...rows]
    for c in [0...cols]
      buildGraph(r,c)

  #simplify graph
  simplifiedGraph = {}

  for key, neighbors of graph
    if neighbors.length == 2
        continue
    
    simplifiedGraph[key] = []
    
    for neighbor in neighbors
      [prev_r, prev_c] = key.split(',').map(Number)
      [curr_r, curr_c] = neighbor
      steps = 1
      
      loop
        next_nodes = graph["#{curr_r},#{curr_c}"].filter(([nr, nc]) -> nr != prev_r or nc != prev_c )

        if next_nodes.length != 1
            break
        
        [prev_r, prev_c] = [curr_r, curr_c]
        [curr_r, curr_c] = next_nodes[0]
        steps += 1

      simplifiedGraph[key].push([[curr_r, curr_c], steps])

  #DFS on simplified graph
  dfs = (node, visited, steps) ->
      
      if "#{node[0]},#{node[1]}" == "#{end[0]},#{end[1]}"
        return steps

      maxSteps = -1

      for [nextNode, edgeWeight] in simplifiedGraph["#{node[0]},#{node[1]}"] ? []
        
        
        if visited.has "#{nextNode[0]},#{nextNode[1]}"
            continue

        visited.add "#{nextNode[0]},#{nextNode[1]}"

        maxSteps = Math.max(maxSteps, dfs(nextNode, visited, steps + edgeWeight))

        visited.delete "#{nextNode[0]},#{nextNode[1]}"

      return maxSteps

    initialVisited = new Set()
    initialVisited.add "#{start[0]},#{start[1]}"
    return dfs(start, initialVisited, 0)

# Read input from input.txt
inputFile = 'input.txt'
input = fs.readFileSync(inputFile, 'utf8').trim().split('\n').map((line) -> line.split(''))

# Solve Part 1
part1Result = solveOptimized(input)
console.log "Part 1: #{part1Result}"

# Solve Part 2
part2Result = solveOptimized(input, true)
console.log "Part 2: #{part2Result}"

