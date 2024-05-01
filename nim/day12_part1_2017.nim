import strutils, sequtils, algorithm

proc dfs(node: int, adj: seq[seq[int]], visited: var seq[bool]) =
  visited[node] = true
  for neighbor in adj[node]:
    if not visited[neighbor]:
      dfs(neighbor, adj, visited)

when isMainModule:
  let file = readFile("input.txt")
  let lines = file.splitLines()
  var adj: seq[seq[int]]
  newSeq(adj, 2000) # assuming max 2000 nodes
  for line in lines:
    let parts = line.split(" <-> ")
    let nodeId = parseInt(parts[0])
    let toNodes = parts[1].split(", ")
    for toNode in toNodes:
      let toNodeId = parseInt(toNode)
      adj[nodeId].add(toNodeId)
      adj[toNodeId].add(nodeId)

  var visited: seq[bool]
  newSeq(visited, 2000) # assuming max 2000 nodes
  dfs(0, adj, visited)

  var count = 0
  for v in visited:
    if v:
      inc count

  echo count