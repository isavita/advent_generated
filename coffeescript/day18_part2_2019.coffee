
fs = require 'fs'

# Priority queue implementation (min-heap)
class PriorityQueue
  constructor: ->
    @heap = []

  enqueue: (item, priority) ->
    @heap.push { item, priority }
    @bubbleUp(@heap.length - 1)

  dequeue: ->
    return null if @isEmpty()

    @swap(0, @heap.length - 1)
    min = @heap.pop()
    @bubbleDown(0)
    min.item

  isEmpty: ->
    @heap.length is 0
    
  peek: ->
      if @isEmpty()
          return null
      else
        return @heap[0].item

  bubbleUp: (index) ->
    parentIndex = Math.floor((index - 1) / 2)
    return if parentIndex < 0 or @heap[parentIndex].priority <= @heap[index].priority

    @swap(index, parentIndex)
    @bubbleUp(parentIndex)

  bubbleDown: (index) ->
    leftIndex = 2 * index + 1
    rightIndex = 2 * index + 2
    smallest = index

    if leftIndex < @heap.length and @heap[leftIndex].priority < @heap[smallest].priority
      smallest = leftIndex
    if rightIndex < @heap.length and @heap[rightIndex].priority < @heap[smallest].priority
      smallest = rightIndex

    return if smallest is index

    @swap(index, smallest)
    @bubbleDown(smallest)

  swap: (i, j) ->
    [@heap[i], @heap[j]] = [@heap[j], @heap[i]]

  length: ->
      @heap.length
    
# Helper function to find neighbors
getNeighbors = (grid, row, col) ->
  neighbors = []
  for [dr, dc] in [[0, 1], [0, -1], [1, 0], [-1, 0]]
    newRow = row + dr
    newCol = col + dc
    if newRow >= 0 and newRow < grid.length and newCol >= 0 and newCol < grid[0].length
      neighbors.push [newRow, newCol]
  neighbors

# Function to find all keys
findAllKeys = (grid) ->
  keys = {}
  for row in [0...grid.length]
    for col in [0...grid[row].length]
      if 'a' <= grid[row][col] <= 'z'
        keys[grid[row][col]] = [row, col]
  keys

# Function to find starting positions
findStarts = (grid) ->
  starts = []
  for row in [0...grid.length]
      for col in [0...grid[row].length]
          if grid[row][col] == '@'
              starts.push [row, col]
  starts
  

# Main function to solve the puzzle
solve = (grid, part2 = false) ->
  starts = findStarts(grid)
  allKeys = findAllKeys(grid)
  numKeys = Object.keys(allKeys).length

  if part2
    # Modify grid for part 2
    [startRow, startCol] = starts[0]  # Original start
    grid[startRow][startCol] = '#'
    grid[startRow - 1][startCol] = '#'
    grid[startRow + 1][startCol] = '#'
    grid[startRow][startCol - 1] = '#'
    grid[startRow][startCol + 1] = '#'
    grid[startRow-1][startCol-1] = '@'
    grid[startRow-1][startCol+1] = '@'
    grid[startRow+1][startCol-1] = '@'
    grid[startRow+1][startCol+1] = '@'

    starts = findStarts(grid)


  initialState = { positions: starts, keys: "", steps: 0 }
  queue = new PriorityQueue()
  queue.enqueue(initialState, 0)
  visited = new Set()
  
  while not queue.isEmpty()
    currentState = queue.dequeue()

    # Check if all keys collected
    if currentState.keys.length is numKeys
      return currentState.steps

    visitedKey = currentState.positions.map((p) -> p.join(',')).join(';') + '|' + currentState.keys.split('').sort().join('')

    continue if visited.has visitedKey
    visited.add visitedKey
    
    for i in [0...currentState.positions.length]
        [row, col] = currentState.positions[i]

        q = new PriorityQueue()
        q.enqueue({pos: [row,col], steps: 0}, 0)
        visitedPos = new Set()

        while not q.isEmpty()
            {pos, steps} = q.dequeue()
            [r, c] = pos
            
            posKey = "#{r},#{c}"
            continue if visitedPos.has(posKey)
            visitedPos.add(posKey)

            cell = grid[r][c]

            # If it's a key we haven't collected
            if ('a' <= cell <= 'z' and currentState.keys.indexOf(cell) is -1)
                newKeys = (currentState.keys + cell).split('').sort().join('')
                newPositions = currentState.positions.slice()
                newPositions[i] = [r, c]
                newState = { positions: newPositions, keys: newKeys, steps: currentState.steps + steps }
                queue.enqueue(newState, newState.steps) # Add heuristic if needed

            else if ('A' <= cell <= 'Z' and currentState.keys.indexOf(cell.toLowerCase()) is -1 )
                continue #we hit a door, and don't have the key
            else if cell isnt '#' #can continue moving from this cell
              for [nr, nc] in getNeighbors(grid, r, c)
                  q.enqueue({pos: [nr, nc], steps: steps + 1}, steps+1) # Manhattan distance as heuristic


# Read input from file
inputFile = 'input.txt'
grid = fs.readFileSync(inputFile, 'utf8').trim().split('\n').map (line) -> line.split('')

# Solve Part 1
part1Result = solve(grid.map((row) -> row.slice())) #deep copy
console.log "Part 1: #{part1Result}"

# Solve Part 2
part2Result = solve(grid.map((row) -> row.slice()), true) #deep copy
console.log "Part 2: #{part2Result}"

