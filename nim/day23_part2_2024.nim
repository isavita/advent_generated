
import sets, tables, strutils, sequtils, algorithm

proc bronKerbosch(r, p, x: var HashSet[string], graph: Table[string, HashSet[string]], bestClique: var seq[string]) =
  if p.len == 0 and x.len == 0:
    if r.len > bestClique.len:
      bestClique = toSeq(r) # Update best clique if current one is larger
    return

  if p.len == 0:
      return

  # Pivoting: Choose pivot u from p union x to reduce recursive calls
  # Simple strategy: pick first element from p
  let pivot = p.items.toSeq[0]
  let pivotNeighbors = graph.getOrDefault(pivot, initHashSet[string]())

  # Iterate over candidates not connected to the pivot: p \ N(pivot)
  let pWithoutPivotNeighbors = p - pivotNeighbors
  let pIter = toSeq(pWithoutPivotNeighbors) # Iterate over a copy

  for v in pIter:
    if v notin p: continue # Check if v is still in the mutable p

    let neighbors = graph.getOrDefault(v, initHashSet[string]())
    var r_new = r
    var p_new = p * neighbors # P intersect N(v)
    var x_new = x * neighbors # X intersect N(v)
    r_new.incl(v)

    bronKerbosch(r_new, p_new, x_new, graph, bestClique)

    p.excl(v) # Move v from P to X
    x.incl(v)


proc main() =
  var graph: Table[string, HashSet[string]]
  var nodes: HashSet[string]

  let content = readFile("input.txt")
  for line in content.strip().splitLines():
    let parts = line.split('-')
    if parts.len == 2:
      let a = parts[0]
      let b = parts[1]
      graph.mgetOrPut(a, initHashSet[string]()).incl(b)
      graph.mgetOrPut(b, initHashSet[string]()).incl(a)
      nodes.incl(a)
      nodes.incl(b)

  var bestClique: seq[string] = @[]
  var r: HashSet[string]
  var p = nodes # Initial candidates: all nodes
  var x: HashSet[string] # Initial excluded: empty

  bronKerbosch(r, p, x, graph, bestClique)

  bestClique.sort(cmp[string])
  echo bestClique.join(",")

when isMainModule:
  main()
