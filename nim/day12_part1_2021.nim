
import std/[strutils, tables, sets]

type
  Cave = ref object
    connections: HashSet[string]

proc newCave(): Cave =
  Cave(connections: initHashSet[string]())

proc connectTo(cave: Cave, name: string) =
  cave.connections.incl(name)

proc countPaths(caves: Table[string, Cave]): int =
  var count = 0

  proc dfs(current: string, visited: var HashSet[string]) =
    if current == "end":
      inc count
      return

    for next in caves[current].connections:
      if next in visited and next.toLowerAscii == next:
        continue

      var visitedCopy = visited
      visitedCopy.incl(next)
      dfs(next, visitedCopy)

  var startVisited = initHashSet[string]()
  startVisited.incl("start")
  dfs("start", startVisited)
  
  result = count

proc main() =
  var caves = initTable[string, Cave]()
  
  for line in lines("input.txt"):
    let paths = line.split("-")
    let (fromCave, toCave) = (paths[0], paths[1])

    if fromCave notin caves:
      caves[fromCave] = newCave()
    if toCave notin caves:
      caves[toCave] = newCave()

    caves[fromCave].connectTo(toCave)
    caves[toCave].connectTo(fromCave)

  echo countPaths(caves)

main()
