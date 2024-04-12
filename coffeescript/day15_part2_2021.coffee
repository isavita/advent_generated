fs = require 'fs'

class Position
  constructor: (@x, @y, @risk) ->

class MinPriorityQueue
  constructor: ->
    @queue = []

  enqueue: (position) ->
    @queue.push position
    @queue.sort (a, b) -> a.risk - b.risk

  dequeue: ->
    @queue.shift()

  isEmpty: ->
    @queue.length == 0

dijkstra = (grid) ->
  pq = new MinPriorityQueue()
  pq.enqueue new Position(0, 0, 0)
  rows = grid.length
  cols = grid[0].length
  dist = Array.from {length: rows}, -> Array(cols).fill(Infinity)
  dist[0][0] = 0
  directions = [{x: 1, y: 0}, {x: 0, y: 1}, {x: -1, y: 0}, {x: 0, y: -1}]
  
  while not pq.isEmpty()
    curr = pq.dequeue()
    continue if curr.x == rows - 1 and curr.y == cols - 1
    for d in directions
      nx = curr.x + d.x
      ny = curr.y + d.y
      if nx >= 0 and ny >= 0 and nx < rows and ny < cols
        nextRisk = curr.risk + grid[nx][ny]
        if nextRisk < dist[nx][ny]
          dist[nx][ny] = nextRisk
          pq.enqueue new Position(nx, ny, nextRisk)
  dist[rows - 1][cols - 1]

extendGrid = (initialGrid) ->
  rows = initialGrid.length
  cols = initialGrid[0].length
  extendedGrid = Array.from {length: rows * 5}, -> Array(cols * 5).fill(0)
  for i in [0...rows * 5]
    for j in [0...cols * 5]
      newRisk = initialGrid[i % rows][j % cols] + Math.floor(i / rows) + Math.floor(j / cols)
      newRisk -= 9 while newRisk > 9
      extendedGrid[i][j] = newRisk
  extendedGrid

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return
  initialGrid = (line.split('').map((ch) -> parseInt ch) for line in data.trim().split('\n'))
  extendedGrid = extendGrid initialGrid
  console.log dijkstra extendedGrid