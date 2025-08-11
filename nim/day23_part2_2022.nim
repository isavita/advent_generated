
import std/[tables, os, sequtils]

type
  P = tuple[x, y: int]

  Elf = object
    pos: P
    moving: bool
    nextPos: P

const
  N = 1
  E = 3
  S = 5
  W = 7

var
  Map: Table[P, bool] = initTable[P, bool]()
  Elves: seq[Elf] = @[]
  CurrDir = 0

const
  Order: array[4, int] = [N, S, W, E]
  Dirs: array[8, P] = [
    (x: -1, y: -1), # NW
    (x: -1, y:  0), # N
    (x: -1, y:  1), # NE
    (x:  0, y:  1), # E
    (x:  1, y:  1), # SE
    (x:  1, y:  0), # S
    (x:  1, y: -1), # SW
    (x:  0, y: -1)  # W
  ]

proc aroundAllEmpty(e: Elf): bool =
  for d in Dirs:
    let adj = (x: e.pos.x + d.x, y: e.pos.y + d.y)
    if Map.hasKey(adj):
      return false
  true

proc elfInDirection(e: Elf, wannaGo: int): bool =
  for j in -1 .. 1:
    let idx = (wannaGo + j + 8) mod 8
    let dxy = Dirs[idx]
    let adj = (x: e.pos.x + dxy.x, y: e.pos.y + dxy.y)
    if Map.hasKey(adj):
      return true
  false

proc run(): bool =
  var proposes: Table[P, int] = initTable[P, int]()
  var someoneMoved = false

  for i in 0 .. Elves.high:
    var e = Elves[i]
    if aroundAllEmpty(e):
      continue
    for j in 0 .. 3:
      let dir = Order[(CurrDir + j) mod 4]
      if elfInDirection(e, dir):
        continue
      let dxy = Dirs[dir]
      let dest = (x: e.pos.x + dxy.x, y: e.pos.y + dxy.y)
      proposes[dest] = proposes.getOrDefault(dest, 0) + 1
      e.nextPos = dest
      e.moving = true
      Elves[i] = e
      break

  for i in 0 .. Elves.high:
    var e = Elves[i]
    if not e.moving:
      continue
    if proposes[e.nextPos] > 1:
      e.moving = false
      Elves[i] = e
      continue
    someoneMoved = true
    Map.del(e.pos)
    Map[e.nextPos] = true
    e.pos = e.nextPos
    e.moving = false
    Elves[i] = e

  CurrDir = (CurrDir + 1) mod 4
  someoneMoved

proc parse() =
  let file = open("input.txt")
  defer: file.close()
  var row = 0
  for line in file.lines:
    for col, ch in line:
      if ch == '#':
        let p = (x: row, y: col)
        Map[p] = true
        Elves.add(Elf(pos: p, moving: false, nextPos: (x: 0, y: 0)))
    inc row

proc main() =
  parse()
  var i = 0
  while true:
    if not run():
      echo(i + 1)
      break
    inc i

main()
