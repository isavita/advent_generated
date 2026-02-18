
import algorithm, sequtils, deques, strutils, os

const DX = [0, -1, 1, 0]
const DY = [-1, 0, 0, 1]

type
  Kind = enum Wall, Space, Elf, Goblin
  Unit = ref object
    kind: Kind
    x, y, hp, atk: int
    alive: bool

var
  m: seq[seq[Kind]]
  unitGrid: seq[seq[Unit]]
  units: seq[Unit]

proc getDistances(sx, sy: int): seq[seq[int]] =
  let h = m.len
  let w = m[0].len
  var d = newSeqWith(h, newSeqWith(w, -1))
  d[sy][sx] = 0
  var q = initDeque[(int, int)]()
  q.addLast((sx, sy))
  while q.len > 0:
    let (cx, cy) = q.popFirst
    for i in 0..3:
      let nx = cx + DX[i]
      let ny = cy + DY[i]
      if ny >= 0 and ny < h and nx >= 0 and nx < w:
        if m[ny][nx] == Space and d[ny][nx] == -1:
          d[ny][nx] = d[cy][cx] + 1
          q.addLast((nx, ny))
  return d

proc move(u: Unit) =
  let enemyKind = if u.kind == Elf: Goblin else: Elf
  for i in 0..3:
    let nx = u.x + DX[i]
    let ny = u.y + DY[i]
    if ny >= 0 and ny < m.len and nx >= 0 and nx < m[0].len:
      if m[ny][nx] == enemyKind: return

  let d = getDistances(u.x, u.y)
  var targets: seq[tuple[x, y, dist: int]]
  for e in units:
    if e.alive and e.kind == enemyKind:
      for i in 0..3:
        let tx = e.x + DX[i]
        let ty = e.y + DY[i]
        if ty >= 0 and ty < m.len and tx >= 0 and tx < m[0].len:
          if m[ty][tx] == Space and d[ty][tx] != -1:
            targets.add((tx, ty, d[ty][tx]))
  
  if targets.len == 0: return
  targets.sort(proc(a, b: tuple[x, y, dist: int]): int =
    if a.dist != b.dist: return cmp(a.dist, b.dist)
    if a.y != b.y: return cmp(a.y, b.y)
    return cmp(a.x, b.x)
  )
  
  let (tx, ty, _) = targets[0]
  let d2 = getDistances(tx, ty)
  var bestStep = (-1, -1)
  var minDist = int.high
  for i in 0..3:
    let nx = u.x + DX[i]
    let ny = u.y + DY[i]
    if ny >= 0 and ny < m.len and nx >= 0 and nx < m[0].len:
      if m[ny][nx] == Space and d2[ny][nx] != -1:
        if d2[ny][nx] < minDist:
          minDist = d2[ny][nx]
          bestStep = (nx, ny)
  
  if bestStep[0] != -1:
    m[u.y][u.x] = Space
    unitGrid[u.y][u.x] = nil
    u.x = bestStep[0]; u.y = bestStep[1]
    m[u.y][u.x] = u.kind
    unitGrid[u.y][u.x] = u

proc attack(u: Unit) =
  let enemyKind = if u.kind == Elf: Goblin else: Elf
  var target: Unit = nil
  for i in 0..3:
    let nx = u.x + DX[i]
    let ny = u.y + DY[i]
    if ny >= 0 and ny < m.len and nx >= 0 and nx < m[0].len:
      if m[ny][nx] == enemyKind:
        let e = unitGrid[ny][nx]
        if target == nil or e.hp < target.hp: target = e
  
  if target != nil:
    target.hp -= u.atk
    if target.hp <= 0:
      target.alive = false
      m[target.y][target.x] = Space
      unitGrid[target.y][target.x] = nil

proc main() =
  let path = "input.txt"
  if not fileExists(path): return
  let lines = readFile(path).strip().splitLines()
  if lines.len == 0: return
  m = newSeqWith(lines.len, newSeq[Kind](lines[0].len))
  unitGrid = newSeqWith(lines.len, newSeq[Unit](lines[0].len))
  for y, line in lines:
    for x, c in line:
      case c
      of '#': m[y][x] = Wall
      of '.': m[y][x] = Space
      of 'E', 'G':
        let k = if c == 'E': Elf else: Goblin
        let u = Unit(kind: k, x: x, y: y, hp: 200, atk: 3, alive: true)
        m[y][x] = k; unitGrid[y][x] = u; units.add(u)
      else: discard

  var rounds = 0
  while true:
    units.sort(proc(a, b: Unit): int =
      if a.y != b.y: return cmp(a.y, b.y)
      return cmp(a.x, b.x)
    )
    var roundFinished = true
    for i in 0..<units.len:
      let u = units[i]
      if not u.alive: continue
      var enemiesLeft = false
      for e in units:
        if e.alive and e.kind != u.kind: enemiesLeft = true; break
      if not enemiesLeft:
        roundFinished = false; break
      move(u); attack(u)
    if not roundFinished: break
    rounds += 1
    
  var totalHP = 0
  for u in units:
    if u.alive: totalHP += u.hp
  echo rounds * totalHP

main()

