fs = require 'fs'

DFS = (node, adj, visited) ->
  visited[node] = true
  for neighbor in adj[node]
    DFS neighbor, adj, visited unless visited[neighbor]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  return console.log "File reading error", err if err

  adj = {}
  lines = data.trim().split '\n'
  for line in lines
    parts = line.split ' <-> '
    from = parseInt parts[0]
    toNodes = parts[1].split ', '

    for toNode in toNodes
      to = parseInt toNode
      adj[from] = (adj[from] or []).concat to
      adj[to] = (adj[to] or []).concat from

  visited = {}
  DFS 0, adj, visited

  count = 0
  count++ for key, value of visited when value
  console.log count