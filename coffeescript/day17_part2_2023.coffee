
fs = require 'fs'

# Priority queue implementation (Min Heap)
class PriorityQueue
  constructor: ->
    @heap = []

  enqueue: (item, priority) ->
    @heap.push { item, priority }
    @bubbleUp(@heap.length - 1)

  dequeue: ->
    return null if @heap.length is 0
    @swap(0, @heap.length - 1)
    min = @heap.pop()
    @bubbleDown(0)
    min.item

  isEmpty: ->
    @heap.length is 0

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


solve = (grid, minSteps, maxSteps) ->
    rows = grid.length
    cols = grid[0].length
    pq = new PriorityQueue()
    pq.enqueue([0, 0, 0, 0], 0)  # [row, col, direction, steps]
    pq.enqueue([0, 0, 1, 0], 0) # start with 2 possible directions. 0: right, 1: down
    visited = new Set()
    distances = {}
    directions = [[0, 1], [1, 0], [0, -1], [-1, 0]] # right, down, left, up

    while not pq.isEmpty()
        [r, c, dir, steps] = pq.dequeue()

        if r is rows - 1 and c is cols - 1 and steps >= minSteps # Reached the destination and moved enough steps
          return distances[[r, c, dir, steps]]

        if visited.has "#{r},#{c},#{dir},#{steps}"
          continue
        visited.add "#{r},#{c},#{dir},#{steps}"
    
        # Try turning
        if steps >= minSteps
          for turn in [-1, 1]
            newDir = (dir + turn + 4) % 4  # Ensure newDir is within [0, 3]
            newR = r + directions[newDir][0]
            newC = c + directions[newDir][1]
            if 0 <= newR < rows and 0 <= newC < cols
              newCost = (distances[[r, c, dir, steps]] or 0) + grid[newR][newC]
              key = [newR, newC, newDir, 1]
              if not distances[key]? or newCost < distances[key]
                distances[key] = newCost
                pq.enqueue(key, newCost)

        # Try going straight
        if steps < maxSteps
          newR = r + directions[dir][0]
          newC = c + directions[dir][1]
          if 0 <= newR < rows and 0 <= newC < cols
            newCost = (distances[[r, c, dir, steps]] or 0) + grid[newR][newC]
            key = [newR, newC, dir, steps + 1]
            if not distances[key]? or newCost < distances[key]
              distances[key] = newCost
              pq.enqueue(key, newCost)

    return -1 # Should not happen with the given input


# Read input from file
inputFile = 'input.txt'
input = fs.readFileSync(inputFile, 'utf8').trim()
grid = input.split('\n').map((line) -> line.split('').map(Number))

# Part 1
part1Result = solve(grid, 0, 3)
console.log "Part 1: #{part1Result}"

# Part 2
part2Result = solve(grid, 4, 10)
console.log "Part 2: #{part2Result}"

