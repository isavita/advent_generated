fs = require 'fs'

readFile = (filename) ->
  fs.readFileSync(filename, 'utf8')

dirs = [
  [0, -1]  # left
  [0, 1]   # right
  [1, 0]   # down
  [-1, 0]  # up
]

bfsGetEdgeWeights = (grid, start) ->
  poiToDistance = {}
  poiToDistance[grid[start[0]][start[1]]] = 0
  queue = [{row: start[0], col: start[1], distance: 0}]
  visited = {}

  while queue.length > 0
    front = queue.shift()
    if visited[[front.row, front.col]]
      continue
    visited[[front.row, front.col]] = true

    if /^\d$/.test(grid[front.row][front.col])
      poiToDistance[grid[front.row][front.col]] = front.distance

    for d in dirs
      nextRow = front.row + d[0]
      nextCol = front.col + d[1]
      if (0 <= nextRow < grid.length) and (0 <= nextCol < grid[nextRow].length) and grid[nextRow][nextCol] != '#'
        queue.push(row: nextRow, col: nextCol, distance: front.distance + 1)

  distances = Array(Object.keys(poiToDistance).length).fill(0)
  for numStr, dist of poiToDistance
    distances[parseInt(numStr, 10)] = dist
  distances

dfs = (graph, entryIndex, visited, returnToZero) ->
  if Object.keys(visited).length == graph.length
    return graph[entryIndex][0] if returnToZero
    return 0

  minDistance = Infinity
  for i in [0...graph.length]
    if not visited[i]
      visited[i] = true
      dist = graph[entryIndex][i] + dfs(graph, i, visited, returnToZero)
      minDistance = Math.min(minDistance, dist)
      delete visited[i]

  minDistance

cleaningRobot = (input) ->
  grid = input.trim().split('\n').map (line) -> line.split('')
  graph = []

  for r in [0...grid.length]
    for c in [0...grid[r].length]
      if /^\d$/.test(grid[r][c])
        poi = grid[r][c]
        distancesFromPOI = bfsGetEdgeWeights(grid, [r, c])
        if graph.length == 0
          for i in [0...distancesFromPOI.length]
            graph.push Array(distancesFromPOI.length).fill(Infinity)
        index = parseInt(poi, 10)
        graph[index] = distancesFromPOI

  dfs(graph, 0, {0: true}, true)

inputData = readFile('input.txt')
result = cleaningRobot(inputData)
console.log result