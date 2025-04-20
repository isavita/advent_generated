
#!/usr/bin/env coffee

fs = require 'fs'

# A simple min‑heap priority queue keyed on the `cost` property
class MinHeap
  constructor: ->
    @heap = []

  push: (node) ->
    @heap.push node
    @siftUp @heap.length - 1

  pop: ->
    return if @heap.length is 0
    root = @heap[0]
    last = @heap.pop()
    if @heap.length > 0
      @heap[0] = last
      @siftDown 0
    root

  size: -> @heap.length

  siftUp: (i) ->
    while i > 0
      p = Math.floor((i - 1) / 2)
      break if @heap[p].cost <= @heap[i].cost
      [@heap[p], @heap[i]] = [@heap[i], @heap[p]]
      i = p

  siftDown: (i) ->
    len = @heap.length
    while true
      l = 2 * i + 1
      r = l + 1
      smallest = i
      if l < len and @heap[l].cost < @heap[smallest].cost
        smallest = l
      if r < len and @heap[r].cost < @heap[smallest].cost
        smallest = r
      break if smallest is i
      [@heap[i], @heap[smallest]] = [@heap[smallest], @heap[i]]
      i = smallest

main = ->

  # Read and parse the grid of heat‐loss digits
  lines = fs.readFileSync 'input.txt', 'utf8'
    .trim()
    .split '\n'
  grid = lines.map (line) -> line.split('').map (d) -> +d

  h = grid.length
  w = grid[0].length

  # Directions: 0=up,1=right,2=down,3=left
  dx = [-1, 0, 1, 0]
  dy = [ 0, 1, 0,-1]

  # Map a state (x,y,dir,k) → a single index in [0..h*w*5*4)
  # dir in -1..3 → dir+1 in 0..4, k in 0..3
  idxOf = (x, y, dir, k) ->
    (((x * w + y) * 5 + (dir + 1)) * 4) + k

  totalStates = h * w * 5 * 4
  # distance array, initialized to Infinity
  dist = new Array totalStates
  dist.fill Infinity

  heap = new MinHeap()

  # Start at (0,0), no direction yet (dir=-1), count=0, cost=0
  startIdx = idxOf(0, 0, -1, 0)
  dist[startIdx] = 0
  heap.push cost: 0, x: 0, y: 0, dir: -1, k: 0

  best = Infinity

  while heap.size() > 0
    {cost, x, y, dir, k} = heap.pop()
    myIdx = idxOf(x, y, dir, k)
    continue if cost > dist[myIdx]

    # If we've reached the bottom‐right, that's the minimal cost
    if x is h-1 and y is w-1
      best = cost
      break

    # Try all four possible next directions
    for nd in [0..3]
      # Determine new consecutive‐move count or skip if invalid
      if dir is -1
        nk = 1
      else if nd is dir
        continue if k >= 3
        nk = k + 1
      else if nd is (dir + 2) % 4
        continue     # no U-turns allowed
      else
        nk = 1       # left or right turn

      nx = x + dx[nd]
      ny = y + dy[nd]
      continue unless 0 <= nx < h and 0 <= ny < w

      ncost = cost + grid[nx][ny]
      ni = idxOf(nx, ny, nd, nk)
      if ncost < dist[ni]
        dist[ni] = ncost
        heap.push cost: ncost, x: nx, y: ny, dir: nd, k: nk

  console.log best

main()
