import os, strutils, tables

type Graph* = Table[string, seq[string]]

proc parseInput(lines: seq[string]): Graph =
  var g: Graph = initTable[string, seq[string]]()
  for line in lines:
    if line.len == 0: discard
    let parts = line.split(": ")
    if parts.len != 2: discard
    let v = parts[0]
    let othersLine = parts[1]
    let others = othersLine.split(" ")
    if not (v in g):
      g[v] = @[]
    for other in others:
      if other == "": discard
      if not (other in g):
        g[other] = @[]
      if not (other in g[v]):
        g[v] = g[v] & @[other]
      if not (v in g[other]):
        g[other] = g[other] & @[v]
  return g

proc copyGraph*(g: Graph): Graph =
  var ng: Graph = initTable[string, seq[string]]()
  for vert, adj in g:
    ng[vert] = @[]
    for nbr in adj:
      ng[vert] = ng[vert] & @[nbr]
  return ng

proc reconstructPath*(start, finish: string, cameFrom: Table[string, string]): seq[string] =
  var path: seq[string] = @[]
  var cur = finish
  while true:
    path = @[cur] & path
    if cur == start: break
    cur = cameFrom[cur]
  return path

proc deleteEdge*(g: var Graph, u, v: string) =
  if (u in g):
    let adj = g[u]
    var newAdj: seq[string] = @[]
    for w in adj:
      if w != v:
        newAdj.add(w)
    g[u] = newAdj

type BFSResult* = tuple[found: bool, cameFrom: Table[string, string]]

proc breadthFirstSearch*(g: Graph, start: string, goal: proc (x: string): bool): BFSResult =
  var frontier: seq[string] = @[start]
  var reached: Table[string, bool] = initTable[string, bool]()
  reached[start] = true
  var cameFrom: Table[string, string] = initTable[string, string]()
  cameFrom[start] = start
  var idx = 0
  while idx < frontier.len:
    let current = frontier[idx]
    inc(idx)
    if goal(current):
      return (true, cameFrom)
    if current in g:
      for neighbor in g[current]:
        if not (neighbor in reached):
          reached[neighbor] = true
          cameFrom[neighbor] = current
          frontier.add(neighbor)
  return (false, cameFrom)

proc main() =
  let content = readFile("input.txt")
  let lines = content.splitLines()
  var graph = parseInput(lines)
  var source = ""
  for k, _ in graph:
    source = k
    break
  if source == "":
    echo 0
    return
  var separateGraph: Graph
  var foundSep = false
  let minCut = 3
  for currentVertex, _ in graph:
    if currentVertex == source: continue
    var newGraph = copyGraph(graph)
    for _ in 1 .. minCut:
      let (found, cameFrom) = breadthFirstSearch(newGraph, source, proc (x: string): bool =
        x == currentVertex
      )
      let path = reconstructPath(source, currentVertex, cameFrom)
      if path.len >= 2:
        for i in 0 ..< (path.len - 1):
          deleteEdge(newGraph, path[i], path[i+1])
    let (exists, _) = breadthFirstSearch(newGraph, source, proc (x: string): bool =
      x == currentVertex
    )
    if not exists:
      separateGraph = newGraph
      foundSep = true
      break
  if not foundSep:
    echo 0
    return
  let (_, cameFromFinal) = breadthFirstSearch(separateGraph, source, proc (x: string): bool = false)
  let len1 = cameFromFinal.len
  let len2 = separateGraph.len - len1
  echo len1 * len2

when isMainModule:
  main()