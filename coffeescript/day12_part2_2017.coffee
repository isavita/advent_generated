
fs = require 'fs'
readline = require 'readline'

# DFS function to traverse the connections
DFS = (node, adj, visited) ->
  visited[node] = true
  for neighbor in adj[node]
    if not visited[neighbor]
      DFS(neighbor, adj, visited)

# Read input file
input = fs.createReadStream('input.txt')
rl = readline.createInterface(input, process.stdout)

# Adjacency list to store connections
adj = {}

# Parse input lines
rl.on 'line', (line) ->
  parts = line.split(' <-> ')
  from = parseInt(parts[0])
  toNodes = parts[1].split(', ')

  toNodes.forEach (toNode) ->
    to = parseInt(toNode)
    if adj[from]
      adj[from].push to
    else
      adj[from] = [to]

    if adj[to]
      adj[to].push from
    else
      adj[to] = [from]

# Set to keep track of visited nodes
rl.on 'close', ->
  visited = {}
  groups = 0

  # Loop through all nodes to find connected components
  for node, neighbors of adj
    if not visited[node]
      DFS(node, adj, visited)
      groups++

  console.log groups
