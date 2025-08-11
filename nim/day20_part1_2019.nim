
import std/[os, strutils, tables, sequtils]

const
  Wall = '#'
  Free = '.'

type
  Point = tuple[x, y: int]

proc isLetter(c: char): bool = c >= 'A' and c <= 'Z'

proc neighbours(p: Point): array[4, Point] =
  [(p.x, p.y + 1), (p.x + 1, p.y), (p.x, p.y - 1), (p.x - 1, p.y)]

proc extractPortal(grid: Table[Point, char], p: Point): (string, Point, bool) =
  let c1 = grid.getOrDefault(p, '\0')
  if isLetter(c1):
    # horizontal
    let c2 = grid.getOrDefault((p.x + 1, p.y), '\0')
    if isLetter(c2):
      let name = $c1 & $c2
      var pt = (p.x + 2, p.y)
      if grid.getOrDefault(pt, '\0') == Free: return (name, pt, true)
      pt = (p.x - 1, p.y)
      if grid.getOrDefault(pt, '\0') == Free: return (name, pt, true)
    # vertical
    let c2v = grid.getOrDefault((p.x, p.y + 1), '\0')
    if isLetter(c2v):
      let name = $c1 & $c2v
      var pt = (p.x, p.y + 2)
      if grid.getOrDefault(pt, '\0') == Free: return (name, pt, true)
      pt = (p.x, p.y - 1)
      if grid.getOrDefault(pt, '\0') == Free: return (name, pt, true)
  ( "", (0,0), false )

type
  Maze = object
    grid: Table[Point, char]
    aa, zz: Point
    teleport: Table[Point, Point]
    isOuter: Table[Point, bool]

proc parse(): Maze =
  var grid = initTable[Point, char]()
  var yMax = 0
  var lines = readFile("input.txt").splitLines()
  for x, line in lines:
    if line.len > yMax: yMax = line.len
    for y, ch in line:
      grid[(x, y)] = ch
  var aa, zz: Point
  var teleport = initTable[Point, Point]()
  var isOuter = initTable[Point, bool]()
  var portalName = initTable[Point, string]()
  var cache = initTable[string, Point]()

  for p, c in grid.pairs:
    if not isLetter(c): continue
    let (name, point, ok) = extractPortal(grid, p)
    if not ok: continue
    portalName[point] = name
    if name == "AA":
      aa = point
      isOuter[point] = true
      continue
    if name == "ZZ":
      zz = point
      isOuter[point] = true
      continue
    if cache.hasKey(name):
      let other = cache[name]
      teleport[point] = other
      teleport[other] = point
    else:
      cache[name] = point
    let (x, y) = point
    if x == 0 or y == 0 or x == lines.len - 2 or y == yMax - 2:
      isOuter[point] = true
    else:
      isOuter[point] = false

  Maze(grid: grid, aa: aa, zz: zz, teleport: teleport, isOuter: isOuter)

proc bfs(m: Maze): int =
  var discovered = initTable[Point, bool]()
  var queue: seq[Point] = @[]
  discovered[m.aa] = true
  queue.add m.aa
  var depth = 0
  while queue.len > 0:
    let levelSize = queue.len
    for i in 0..<levelSize:
      let curr = queue[0]
      queue.delete(0)
      if curr == m.zz: return depth
      for n in curr.neighbours():
        let dest = m.grid.getOrDefault(n, '\0')
        if dest == Wall or dest == '\0': continue
        if dest == Free:
          if not discovered.hasKey(n):
            discovered[n] = true
            queue.add n
        elif isLetter(dest):
          let nxt = m.teleport.getOrDefault(curr, (0,0))
          if not discovered.hasKey(nxt):
            discovered[nxt] = true
            queue.add nxt
    inc depth
  -1

when isMainModule:
  let m = parse()
  echo bfs(m)
