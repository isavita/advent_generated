
import std/[strutils, sequtils, deques, sets]

type Node = object
  x, y, size, used, avail: int

proc main() =
  let lines = readFile("input.txt").strip().splitLines()
  var nodes: seq[Node] = @[]
  for i in 2..<lines.len:
    let p = lines[i].splitWhitespace()
    if p.len < 4: continue
    let c = p[0].split('-')
    nodes.add(Node(x: c[1][1..^1].parseInt, y: c[2][1..^1].parseInt,
                   size: p[1][0..^2].parseInt, used: p[2][0..^2].parseInt,
                   avail: p[3][0..^2].parseInt))

  var v = 0
  for i in 0..<nodes.len:
    if nodes[i].used == 0: continue
    for j in 0..<nodes.len:
      if i != j and nodes[i].used <= nodes[j].avail: inc v
  echo "Part 1: ", v

  let mx = nodes.mapIt(it.x).max
  let my = nodes.mapIt(it.y).max
  let e = nodes.filterIt(it.used == 0)[0]
  let walls = nodes.filterIt(it.used > e.size).mapIt((it.x, it.y)).toHashSet()

  var q = initDeque[tuple[x, y, d: int]]()
  q.addLast((e.x, e.y, 0))
  var visited = [(e.x, e.y)].toHashSet
  var dToT = 0
  while q.len > 0:
    let curr = q.popFirst()
    if curr.x == mx - 1 and curr.y == 0:
      dToT = curr.d
      break
    for step in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
      let (nx, ny) = (curr.x + step[0], curr.y + step[1])
      if nx in 0..mx and ny in 0..my and (nx, ny) notin walls and (nx, ny) notin visited:
        visited.incl((nx, ny))
        q.addLast((nx, ny, curr.d + 1))
  
  echo "Part 2: ", dToT + 1 + 5 * (mx - 1)

main()
