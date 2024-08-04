fs = require 'fs'

readFile = (path) ->
  fs.readFileSync(path, 'utf8').trim()

dirs = [
  [0, -1]
  [0, 1]
  [1, 0]
  [-1, 0]
]

minInt = (a, b) ->
  if a < b then a else b

bfsGetEdgeWeights = (grid, start) ->
  poiToDistance = {}
  poiToDistance[grid[start[0]][start[1]]] = 0

  queue = [{ row: start[0], col: start[1], distance: 0 }]
  visited = {}

  while queue.length > 0
    front = queue.shift()
    unless visited["#{front.row},#{front.col}"]
      visited["#{front.row},#{front.col}"] = true

      if /[0-9]/.test(grid[front.row][front.col])
        poiToDistance[grid[front.row][front.col]] = front.distance

      for dir in dirs
        nextRow = front.row + dir[0]
        nextCol = front.col + dir[1]
        unless grid[nextRow]?[nextCol] is '#'
          queue.push {
            row: nextRow
            col: nextCol
            distance: front.distance + 1
          }

  distances = new Array(Object.keys(poiToDistance).length)
  for numStr, dist of poiToDistance
    n = parseInt(numStr)
    distances[n] = dist

  distances

dfs = (graph, entryIndex, visited, returnToZero) ->
  if Object.keys(visited).length is graph.length
    if returnToZero
      return graph[entryIndex][0]
    else
      return 0

  minDistance = Infinity
  for i, val of graph[entryIndex]
    unless visited[i]
      visited[i] = true
      dist = val + dfs(graph, i, visited, returnToZero)
      minDistance = minInt(minDistance, dist)
      delete visited[i]

  minDistance

cleaningRobot = (input) ->
  grid = input.split('\n').map (l) -> l.split ''

  graph = null
  for row, r in grid
    for cell, c in row
      if /[0-9]/.test(cell)
        poi = cell
        distancesFromPOI = bfsGetEdgeWeights(grid, [r, c])
        unless graph
          graph = new Array(distancesFromPOI.length)
          for i in [0...distancesFromPOI.length]
            graph[i] = new Array(distancesFromPOI.length)

        index = parseInt(poi)
        graph[index] = distancesFromPOI

  dfs(graph, 0, {0: true}, false)

console.log cleaningRobot(readFile('input.txt'))