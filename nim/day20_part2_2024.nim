
import std/[strutils, sequtils, deques, tables, os]

const MaxCheatDist = 20
const CheatThreshold = 100

type Coord = tuple[r, c: int]
type Grid = seq[seq[char]]
type Walls = seq[seq[bool]]
type DistGrid = seq[seq[int]]
type CheatKey = tuple[start, finish: Coord]

proc bfs(start: Coord; h, w: int; walls: Walls): DistGrid =
  result = newSeqWith(h, newSeq[int](w))
  for r in 0..<h:
    for c in 0..<w:
      result[r][c] = -1

  var q = initDeque[Coord]()

  result[start.r][start.c] = 0
  q.addLast(start)

  let dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

  while q.len > 0:
    let curr = q.popFirst()
    let d = result[curr.r][curr.c]

    for (dr, dc) in dirs:
      let nr = curr.r + dr
      let nc = curr.c + dc

      if nr >= 0 and nr < h and nc >= 0 and nc < w and not walls[nr][nc] and result[nr][nc] == -1:
        result[nr][nc] = d + 1
        q.addLast((nr, nc))

proc isTrack(r, c: int; h, w: int; walls: Walls): bool =
  r >= 0 and r < h and c >= 0 and c < w and not walls[r][c]

proc main() =
  let fileContent = readFile("input.txt")
  let grid = fileContent.strip.splitLines
  if grid.len == 0:
    echo 0
    return

  let h = grid.len
  let w = grid[0].len

  var walls = newSeqWith(h, newSeq[bool](w))
  var trackCells = newSeq[Coord]()
  var S, E: Coord

  for r in 0..<h:
    for c in 0..<w:
      case grid[r][c]
      of '#':
        walls[r][c] = true
      of 'S':
        S = (r, c)
        trackCells.add((r, c))
      of 'E':
        E = (r, c)
        trackCells.add((r, c))
      else: # Treat other characters as track cells
        trackCells.add((r, c))

  let distFromS = bfs(S, h, w, walls)
  let distFromE = bfs(E, h, w, walls)

  let normalCost = distFromS[E.r][E.c]
  if normalCost == -1:
    echo 0
    return

  var cheats = initTable[CheatKey, int]()
  let dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

  for startCoord in trackCells:
    let sd = distFromS[startCoord.r][startCoord.c]
    if sd == -1: continue

    var distC = newSeqWith(h, newSeq[int](w))
    for r in 0..<h:
      for c in 0..<w:
        distC[r][c] = -1

    var q = initDeque[Coord]()
    distC[startCoord.r][startCoord.c] = 0
    q.addLast(startCoord)

    while q.len > 0:
      let curr = q.popFirst()
      let steps = distC[curr.r][curr.c]

      if steps == MaxCheatDist: continue

      for (dr, dc) in dirs:
        let nr = curr.r + dr
        let nc = curr.c + dc

        if nr >= 0 and nr < h and nc >= 0 and nc < w and distC[nr][nc] == -1:
           # Note: Inner BFS does *not* check walls during traversal in the original Python
           distC[nr][nc] = steps + 1
           q.addLast((nr, nc))

    for r in 0..<h:
      for c in 0..<w:
        let s = distC[r][c]
        if s > 0 and s <= MaxCheatDist and isTrack(r, c, h, w, walls):
          let ed = distFromE[r][c]
          if ed == -1: continue

          let cost = sd + s + ed
          if cost < normalCost:
            let key = (start: startCoord, finish: (r, c))
            if not cheats.contains(key) or cost < cheats[key]:
              cheats[key] = cost

  var count = 0
  for cost in cheats.values:
    if normalCost - cost >= CheatThreshold:
      count.inc

  echo count

when isMainModule:
  main()
