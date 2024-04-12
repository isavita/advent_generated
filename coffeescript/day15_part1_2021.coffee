fs = require 'fs'

class PriorityQueue
  constructor: ->
    @data = []

  push: (item) ->
    @data.push item
    @data.sort (a, b) -> a.risk - b.risk

  pop: ->
    @data.shift()

  isEmpty: ->
    @data.length == 0

dijkstra = (grid) ->
  pq = new PriorityQueue()
  pq.push {x: 0, y: 0, risk: 0}
  rows = grid.length
  cols = grid[0].length
  dist = (Array(rows).fill().map -> Array(cols).fill(Infinity))
  dist[0][0] = 0
  directions = [{x: 1, y: 0}, {x: 0, y: 1}, {x: -1, y: 0}, {x: 0, y: -1}]

  while not pq.isEmpty()
    curr = pq.pop()
    if curr.x == rows - 1 and curr.y == cols - 1
      return curr.risk
    for d in directions
      nx = curr.x + d.x
      ny = curr.y + d.y
      if nx >= 0 and ny >= 0 and nx < rows and ny < cols
        nextRisk = curr.risk + grid[nx][ny]
        if nextRisk < dist[nx][ny]
          dist[nx][ny] = nextRisk
          pq.push {x: nx, y: ny, risk: nextRisk}

  return -1

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log 'Error reading file:', err
    return

  grid = data.trim().split('\n').map (line) -> line.split('').map (ch) -> parseInt ch
  console.log dijkstra(grid)