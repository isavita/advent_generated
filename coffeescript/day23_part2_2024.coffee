
fs = require 'fs'

graph = {}
bestClique = []

intersect = (a, b) ->
  out = []
  for x in a
    if b[x]
      out.push x
  out

union = (a, x) ->
  a.concat [x]

remove = (slice, s) ->
  (x for x in slice when x != s)

neighborsOf = (node) ->
  graph[node] or {}

bronKerbosch = (R, P, X) ->
  if P.length == 0 and X.length == 0
    if R.length > bestClique.length
      bestClique = R.slice()
    return
  tempP = P.slice()
  for v in tempP
    neighbors = neighborsOf v
    bronKerbosch(
      union(R, v),
      intersect(P, neighbors),
      intersect(X, neighbors)
    )
    P = remove(P, v)
    X = union(X, v)

try
  data = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
  nodesSet = {}
  for line in data
    parts = line.split('-')
    if parts.length != 2
      continue
    [a, b] = parts
    graph[a] ?= {}
    graph[b] ?= {}
    graph[a][b] = true
    graph[b][a] = true
    nodesSet[a] = true
    nodesSet[b] = true
  allNodes = Object.keys(nodesSet)
  bronKerbosch([], allNodes, [])
  bestClique.sort()
  console.log bestClique.join(',')
catch error
  console.error error
