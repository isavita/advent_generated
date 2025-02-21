
fs = require 'fs'

# Dijkstra's algorithm for shortest paths
dijkstra = (graph, start) ->
  distances = {}
  for node of graph
    distances[node] = Infinity
  distances[start] = 0
  queue = [start]
  visited = {}

  while queue.length
    current = queue.shift()
    visited[current] = true

    for neighbor, weight of graph[current]
      alt = distances[current] + weight
      if alt < distances[neighbor]
        distances[neighbor] = alt
        if not visited[neighbor]
          queue.push neighbor
          # Simple priority queue:  sort each time to put nearest nodes first.
          queue.sort (a, b) -> distances[a] - distances[b]


  distances
# Calculates maximum pressure released
calculateMaxPressure = (valves, distances, timeLimit) ->
  maxPressure = 0

  dfs = (currentValve, timeLeft, openedValves, currentPressure) ->
    maxPressure = Math.max(maxPressure, currentPressure)

    for nextValve of valves
      if not openedValves[nextValve] and valves[nextValve].flow > 0
        timeToReach = distances[currentValve][nextValve] + 1 # +1 to open
        if timeLeft >= timeToReach
          newOpened = Object.assign({}, openedValves)
          newOpened[nextValve] = true
          newPressure = currentPressure + (timeLeft - timeToReach) * valves[nextValve].flow
          dfs(nextValve, timeLeft - timeToReach, newOpened, newPressure)


  dfs('AA', timeLimit, {}, 0)
  maxPressure


# Read input and parse data
solve = (filePath) ->
    input = fs.readFileSync(filePath, 'utf8').trim().split('\n')

    valves = {}
    allValves = [] # For building the full graph

    for line in input
      match = line.match /Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)/
      valveName = match[1]
      flowRate = parseInt(match[2])
      neighbors = match[3].split(', ')
      valves[valveName] = { flow: flowRate, neighbors: neighbors }
      allValves.push(valveName)

    # Build the full graph with edge weights of 1 (for Dijkstra)
    fullGraph = {}
    for valveName in allValves
      fullGraph[valveName] = {}
      for neighbor in valves[valveName].neighbors
        fullGraph[valveName][neighbor] = 1

    # Calculate shortest path distances between all relevant valves
    distances = {}
    for valve1 in allValves
        distances[valve1] = dijkstra(fullGraph, valve1)
    
    maxPressure = calculateMaxPressure(valves, distances, 30)
    console.log "Maximum pressure released: #{maxPressure}"

solve 'input.txt'
