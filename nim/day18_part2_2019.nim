
import std/[os, strutils, tables, heapqueue, hashes]

type
  Point = tuple[x, y: int8]
  State = object
    p: array[4, Point]
    mask: int32
    worker_id: int
  Node = object
    dist: int
    state: State

proc hash(s: State): Hash =
  var h: Hash = 0
  h = h !& hash(s.p)
  h = h !& hash(s.mask)
  h = h !& hash(s.worker_id)
  result = !$h

proc `<`(a, b: Node): bool =
  a.dist < b.dist

proc solve() =
  if not fileExists("input.txt"): return
  var grid = newSeq[string]()
  for line in lines("input.txt"):
    if line.strip().len > 0:
      grid.add(line)

  var 
    startPos: Point
    allKeysMask: int32 = 0

  for y in 0..<grid.len:
    for x in 0..<grid[y].len:
      let c = grid[y][x]
      if c >= 'a' and c <= 'z':
        allKeysMask = allKeysMask or (1 shl (ord(c) - ord('a'))).int32
      elif c == '@':
        startPos = (x.int8, y.int8)

  grid[startPos.y][startPos.x] = '#'
  grid[startPos.y-1][startPos.x] = '#'
  grid[startPos.y+1][startPos.x] = '#'
  grid[startPos.y][startPos.x-1] = '#'
  grid[startPos.y][startPos.x+1] = '#'
  
  let starts: array[4, Point] = [
    (startPos.x - 1, startPos.y - 1),
    (startPos.x - 1, startPos.y + 1),
    (startPos.x + 1, startPos.y - 1),
    (startPos.x + 1, startPos.y + 1)
  ]
  
  for i in 0..3:
    grid[starts[i].y][starts[i].x] = '@'

  var dists = initTable[State, int]()
  var pq = HeapQueue[Node]()

  for i in 0..3:
    let s = State(p: starts, mask: allKeysMask, worker_id: i)
    dists[s] = 0
    pq.push(Node(dist: 0, state: s))

  let dx = [0.int8, 0, 1, -1]
  let dy = [1.int8, -1, 0, 0]

  while pq.len > 0:
    let node = pq.pop()
    if node.dist > dists.getOrDefault(node.state, int.high): continue
    if node.state.mask == 0:
      echo node.dist
      return

    let wid = node.state.worker_id
    let cp = node.state.p[wid]

    for i in 0..3:
      let nx = cp.x + dx[i]
      let ny = cp.y + dy[i]
      
      if ny < 0 or ny >= grid.len.int8 or nx < 0 or nx >= grid[ny].len.int8: continue
      let cell = grid[ny][nx]
      if cell == '#': continue
      if cell >= 'A' and cell <= 'Z':
        if (node.state.mask and (1 shl (ord(cell) - ord('A'))).int32) != 0: continue

      var nS = node.state
      nS.p[wid] = (nx, ny)
      let nD = node.dist + 1
      var found = false

      if cell >= 'a' and cell <= 'z':
        let bit = (1 shl (ord(cell) - ord('a'))).int32
        if (nS.mask and bit) != 0:
          found = true
          nS.mask = nS.mask and (not bit)

      if found:
        for j in 0..3:
          var ns2 = nS
          ns2.worker_id = j
          if nD < dists.getOrDefault(ns2, int.high):
            dists[ns2] = nD
            pq.push(Node(dist: nD, state: ns2))
      else:
        if nD < dists.getOrDefault(nS, int.high):
          dists[nS] = nD
          pq.push(Node(dist: nD, state: nS))

solve()

