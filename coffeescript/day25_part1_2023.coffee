fs = require 'fs'

class Edge
  constructor: (@start, @end, @weight) ->

parseInput = (input) ->
  weight = 1
  graph = {}

  for line in input
    [vertice, others] = line.split(': ')
    others = others.split ' '

    graph[vertice] ?= []
    for other in others
      graph[other] ?= []
      graph[vertice].push new Edge(vertice, other, weight)
      graph[other].push new Edge(other, vertice, weight)

  graph

breadthFirstSearch = (graph, start, goalFunc) ->
  frontier = [start]
  reached = {}
  reached[start] = true
  cameFrom = {}
  cameFrom[start] = start

  while frontier.length > 0
    current = frontier.shift()
    return [true, cameFrom] if goalFunc(current)

    for edge in graph[current]
      if not (edge.end of reached)
        frontier.push edge.end
        reached[edge.end] = true
        cameFrom[edge.end] = current

  [false, cameFrom]

reconstructPath = (start, end, cameFrom) ->
  path = []
  current = end
  while current isnt start
    path.unshift current
    current = cameFrom[current]
  path.unshift start
  path

copyGraph = (graph) ->
  newGraph = {}
  for vertice, edges of graph
    newGraph[vertice] = edges.slice()

  newGraph

solve = (input) ->
  minCut = 3
  graph = parseInput(input)

  source = null
  for vertice of graph
    source = vertice
    break

  separateGraph = null
  for end of graph
    continue if source == end

    newGraph = copyGraph(graph)
    for i in [0...minCut]
      [_, cameFrom] = breadthFirstSearch(newGraph, source, (vertice) -> vertice == end)
      path = reconstructPath(source, end, cameFrom)
      for j in [0...path.length - 1]
        startNode = path[j]
        endNode = path[j+1]
        newGraph[startNode] = newGraph[startNode].filter((edge) -> edge.end isnt endNode)
        newGraph[endNode] = newGraph[endNode].filter((edge) -> edge.end isnt startNode)

    [isValid, _] = breadthFirstSearch(newGraph, source, (vertice) -> vertice == end)
    if not isValid
      separateGraph = newGraph
      break

  separateGraph ?= graph  # Default to original graph if no valid separateGraph found
  [_, cameFrom] = breadthFirstSearch(separateGraph, source, (vertice) -> false)
  length1 = Object.keys(cameFrom).length
  length2 = Object.keys(separateGraph).length - length1

  length1 * length2

readFile = (fileName) ->
  input = fs.readFileSync(fileName, 'utf8').trim().split '\n'
  solve(input)

console.log readFile('input.txt')