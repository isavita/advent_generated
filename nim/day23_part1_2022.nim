import os
import strutils

type Dir = enum N, S, W, E
type Pos = tuple[x: int, y: int]

type Group = object
  pos: Pos
  movers: seq[Pos]

proc inList(list: seq[Pos], p: Pos): bool =
  for v in list:
    if v.x == p.x and v.y == p.y:
      return true
  false

proc inElves(elves: seq[Pos], p: Pos): bool =
  for e in elves:
    if e.x == p.x and e.y == p.y:
      return true
  false

proc absInt(a: int): int =
  if a >= 0: a else: -a

proc checkAdjacent(elves: seq[Pos], pos: Pos): bool =
  for e in elves:
    if e.x == pos.x and e.y == pos.y:
      continue
    if absInt(e.x - pos.x) <= 1 and absInt(e.y - pos.y) <= 1:
      return false
  true

proc movePos(pos: Pos, d: Dir): Pos =
  case d
  of N: (pos.x, pos.y - 1)
  of S: (pos.x, pos.y + 1)
  of W: (pos.x - 1, pos.y)
  of E: (pos.x + 1, pos.y)
  else: pos

proc checkDirection(elves: seq[Pos], pos: Pos, dir: Dir): bool =
  case dir
  of N:
    not inElves(elves, (pos.x - 1, pos.y - 1)) and
    not inElves(elves, (pos.x, pos.y - 1)) and
    not inElves(elves, (pos.x + 1, pos.y - 1))
  of S:
    not inElves(elves, (pos.x - 1, pos.y + 1)) and
    not inElves(elves, (pos.x, pos.y + 1)) and
    not inElves(elves, (pos.x + 1, pos.y + 1))
  of W:
    not inElves(elves, (pos.x - 1, pos.y - 1)) and
    not inElves(elves, (pos.x - 1, pos.y)) and
    not inElves(elves, (pos.x - 1, pos.y + 1))
  of E:
    not inElves(elves, (pos.x + 1, pos.y - 1)) and
    not inElves(elves, (pos.x + 1, pos.y)) and
    not inElves(elves, (pos.x + 1, pos.y + 1))
  else: true

proc parseInput(filename: string): seq[Pos] =
  var elves: seq[Pos] = @[]
  let content = readFile(filename)
  let lines = content.splitLines()
  for y in 0 ..< lines.len:
    let line = lines[y]
    for x in 0 ..< line.len:
      if line[x] == '#':
        elves.add((x, y))
  return elves

proc simulateRound(elves: var seq[Pos], dirs: var seq[Dir]): bool =
  var groups: seq[Group] = @[]
  for elf in elves:
    if checkAdjacent(elves, elf):
      continue
    for d in dirs:
      if checkDirection(elves, elf, d):
        let p = movePos(elf, d)
        var idx = -1
        for i in 0 ..< groups.len:
          if groups[i].pos.x == p.x and groups[i].pos.y == p.y:
            idx = i
            break
        if idx == -1:
          groups.add(Group(pos: p, movers: @[elf]))
        else:
          groups[idx].movers.add(elf)
        break
      else:
        discard
  var movedList: seq[Pos] = @[]
  var newElves: seq[Pos] = @[]
  for g in groups:
    if g.movers.len == 1:
      movedList.add(g.movers[0])
      newElves.add(g.pos)
  for elf in elves:
    if not inList(movedList, elf):
      newElves.add(elf)
  elves = newElves
  var newDirs: seq[Dir] = @[]
  if dirs.len > 1:
    for i in 1 ..< dirs.len:
      newDirs.add(dirs[i])
  if dirs.len > 0:
    newDirs.add(dirs[0])
  dirs = newDirs
  movedList.len > 0

proc countEmptyGround(elves: seq[Pos]): int =
  var minX = elves[0].x
  var maxX = elves[0].x
  var minY = elves[0].y
  var maxY = elves[0].y
  for e in elves:
    if e.x < minX: minX = e.x
    if e.x > maxX: maxX = e.x
    if e.y < minY: minY = e.y
    if e.y > maxY: maxY = e.y
  let area = (maxX - minX + 1) * (maxY - minY + 1)
  area - elves.len

proc solvePart1(filename: string): int =
  var elves = parseInput(filename)
  var directions: seq[Dir] = @[N, S, W, E]
  for _ in 0 ..< 10:
    discard simulateRound(elves, directions)
  countEmptyGround(elves)

when isMainModule:
  let ans = solvePart1("input.txt")
  echo ans