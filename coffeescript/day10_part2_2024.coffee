
fs = require 'fs'

# Function to find all reachable '9's from a starting '0' (trailhead) for Part 1.
findReachableNines = (grid, startRow, startCol) ->
  rows = grid.length
  cols = grid[0].length
  visited = Array(rows).fill(null).map(-> Array(cols).fill(false))
  reachableNines = 0
  queue = [[startRow, startCol, 0]]  # [row, col, height]
  visited[startRow][startCol] = true

  while queue.length > 0
    [row, col, height] = queue.shift()

    if height is 9
      reachableNines++
      continue

    # Explore adjacent cells (up, down, left, right).
    for [dr, dc] in [[-1, 0], [1, 0], [0, -1], [0, 1]]
      newRow = row + dr
      newCol = col + dc

      # Check boundaries and height difference.
      if 0 <= newRow < rows and 0 <= newCol < cols and not visited[newRow][newCol]
        newHeight = parseInt(grid[newRow][newCol])
        if newHeight is height + 1
          visited[newRow][newCol] = true
          queue.push([newRow, newCol, newHeight])
  reachableNines


# Function to count distinct hiking trails for Part 2
countDistinctTrails = (grid, startRow, startCol) ->
    rows = grid.length
    cols = grid[0].length
    visited = {} #Use object as hash map

    dfs = (row, col, height) ->
        key = "#{row},#{col}"

        if height is 9
           return 1

        if visited[key]?
            return 0

        visited[key] = true

        count = 0

        for [dr, dc] in [[-1, 0], [1, 0], [0, -1], [0, 1]]
            newRow = row + dr
            newCol = col + dc

            #Check out of bounds
            if 0 <= newRow < rows and 0 <= newCol < cols
                newHeight = parseInt(grid[newRow][newCol])
                if newHeight is height + 1
                    count += dfs(newRow, newCol, newHeight)
        delete visited[key]
        return count
    return dfs(startRow, startCol, 0)


# Read input from file.
try
  data = fs.readFileSync('input.txt', 'utf8')
  grid = data.trim().split('\n').map((line) -> line.trim())

  rows = grid.length
  cols = grid[0].length

  # Part 1: Sum of scores
  totalScore = 0
  for r in [0...rows]
    for c in [0...cols]
      if grid[r][c] is '0'
        totalScore += findReachableNines(grid, r, c)
  console.log "Part 1: #{totalScore}"


  # Part 2: Sum of ratings.
  totalRating = 0
  for r in [0...rows]
    for c in [0...cols]
      if grid[r][c] is '0'
          totalRating += countDistinctTrails(grid,r,c)

  console.log "Part 2: #{totalRating}"


catch err
  console.error 'Error reading file:', err

