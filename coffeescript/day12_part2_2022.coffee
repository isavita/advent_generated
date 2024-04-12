fs = require 'fs'

class PriorityQueue
  constructor: ->
    @items = []

  push: (obj, priority) ->
    item = {obj, priority}
    @items.push item
    @items.sort (a, b) -> a.priority - b.priority  # Sort ascending by priority

  pop: ->
    @items.shift().obj

  isEmpty: ->
    @items.length == 0

dijkstra = (grid, end) ->
  pq = new PriorityQueue()
  dist = {}
  dist[JSON.stringify(end)] = 0
  pq.push(end, 0)

  Neighbors4 = [{x: 0, y: 1}, {x: 0, y: -1}, {x: 1, y: 0}, {x: -1, y: 0}]

  while not pq.isEmpty()
    curr = pq.pop()
    for n in Neighbors4
      next = {x: curr.x + n.x, y: curr.y + n.y}
      nextKey = JSON.stringify(next)
      currKey = JSON.stringify(curr)
      continue unless grid[nextKey]
      continue if (grid[currKey].charCodeAt(0) - grid[nextKey].charCodeAt(0)) > 1
      nextDist = dist[currKey] + 1
      if not dist[nextKey] or nextDist < dist[nextKey]
        dist[nextKey] = nextDist
        pq.push(next, nextDist)

  dist

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  grid = {}
  start = undefined
  end = undefined
  as = []

  lines = data.split '\n'
  for y in [0...lines.length]
    for x in [0...lines[y].length]
      p = {x, y}
      pKey = JSON.stringify(p)
      grid[pKey] = lines[y][x]
      start = p if lines[y][x] == 'S'
      end = p if lines[y][x] == 'E'
      as.push p if lines[y][x] == 'a'

  startKey = JSON.stringify(start)
  endKey = JSON.stringify(end)

  grid[startKey] = 'a'
  grid[endKey] = 'z'

  dists = dijkstra(grid, end)

  l = dists[startKey]
  for a in as
    aKey = JSON.stringify(a)
    if dists[aKey]
      l = Math.min(l, dists[aKey])

  console.log l