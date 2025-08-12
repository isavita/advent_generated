
import std/[strutils, tables, os]

proc walk(graph: Table[string, seq[string]], current: string,
          visited: var Table[string, int], doubleUsed: bool): int =
  if current == "end":
    return 1
  visited[current] = visited.getOrDefault(current, 0) + 1
  var total = 0
  for next in graph[current]:
    if next == "start":
      continue
    let isSmall = next != next.toUpper()
    var newDoubleUsed = doubleUsed
    if isSmall and visited.getOrDefault(next, 0) > 0:
      if doubleUsed:
        continue
      else:
        newDoubleUsed = true
    total += walk(graph, next, visited, newDoubleUsed)
  visited[current] = visited.getOrDefault(current, 0) - 1
  return total

proc main() =
  let raw = readFile("input.txt").strip()
  var pairs: seq[tuple[a, b: string]] = @[]
  for line in raw.splitLines():
    let parts = line.split("-")
    pairs.add((parts[0], parts[1]))
  var graph = initTable[string, seq[string]]()
  for p in pairs:
    graph.mgetOrPut(p.a, @[]).add(p.b)
    graph.mgetOrPut(p.b, @[]).add(p.a)
  var visited = initTable[string, int]()
  let result = walk(graph, "start", visited, false)
  echo result

main()
