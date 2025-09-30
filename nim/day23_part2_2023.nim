
import std/[strutils, sets, tables, deques, algorithm]

type
  Coord = object
    x, y: int
  Edge = object
    start, last: Coord
    weight: int
  Grid = object
    width, height: int
    data: Table[Coord, char]
  Graph = object
    vertices: HashSet[Coord]
    edges: Table[Coord, HashSet[Edge]]

proc `<`(a, b: Coord): bool =
  if a.y != b.y: a.y < b.y else: a.x < b.x

proc `==`(a, b: Coord): bool = a.x == b.x and a.y == b.y
proc `+`(a, b: Coord): Coord = Coord(x: a.x + b.x, y: a.y + b.y)

let
  North = Coord(x: 0, y: -1)
  South = Coord(x: 0, y: 1)
  West  = Coord(x: -1, y: 0)
  East  = Coord(x: 1, y: 0)

proc parseInput(lines: seq[string]): Grid =
  result.height = lines.len
  result.width = if lines.len > 0: lines[0].len else: 0
  for y in 0 ..< result.height:
    for x in 0 ..< result.width:
      let c = lines[y][x]
      if c != '.': result.data[Coord(x: x, y: y)] = c

proc inBounds(g: Grid; c: Coord): bool =
  c.x >= 0 and c.x < g.width and c.y >= 0 and c.y < g.height

proc valid(g: Grid; c: Coord): bool =
  g.inBounds(c) and g.data.getOrDefault(c) != '#'

proc neighbors4(g: Grid; c: Coord): seq[Coord] =
  for d in [North, South, West, East]:
    let n = c + d
    if g.valid(n): result.add n

proc getEdgesBfs(g: Grid; start: Coord; verts: HashSet[Coord]): HashSet[Edge] =
  var
    frontier = initDeque[Coord]()
    reached: HashSet[Coord]
    dist: Table[Coord, int]
  frontier.addLast(start)
  reached.incl(start)
  dist[start] = 0
  while frontier.len > 0:
    let cur = frontier.popFirst()
    if cur in verts and cur != start:
      result.incl Edge(start: start, last: cur, weight: dist[cur])
      continue
    for n in g.neighbors4(cur):
      if n notin reached:
        reached.incl(n)
        dist[n] = dist[cur] + 1
        frontier.addLast(n)

proc getGraph(g: Grid; start, last: Coord): Graph =
  result.vertices.incl(start)
  result.vertices.incl(last)
  for y in 0 ..< g.height:
    for x in 0 ..< g.width:
      let c = Coord(x: x, y: y)
      if g.valid(c) and g.neighbors4(c).len > 2:
        result.vertices.incl(c)
  for v in result.vertices:
    result.edges[v] = g.getEdgesBfs(v, result.vertices)

proc maxDist(g: Grid; graph: Graph; cur, last: Coord; seen: var HashSet[Coord]): int =
  if cur == last: return 0
  seen.incl(cur)
  result = -1
  for e in graph.edges[cur]:
    if e.last notin seen:
      let d = g.maxDist(graph, e.last, last, seen)
      if d != -1: result = max(result, d + e.weight)
  seen.excl(cur)

proc solve(lines: seq[string]): int =
  let g = parseInput(lines)
  let start = Coord(x: 1, y: 0)
  let last  = Coord(x: g.width - 2, y: g.height - 1)
  let graph = g.getGraph(start, last)
  var seen: HashSet[Coord]
  result = g.maxDist(graph, start, last, seen)

let ans = solve(readFile("input.txt").splitLines)
echo ans
