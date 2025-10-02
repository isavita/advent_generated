
import std/[strutils, tables, deques]

const Wall = '#'
const Free = '.'

type
  Pos = tuple[x, y: int16]
  Map = object
    xMax, yMax: int16
    grid: Table[Pos, char]
    aa, zz: Pos
    teleport: Table[Pos, Pos]
    portalName: Table[Pos, string]
    isOuter: Table[Pos, bool]

proc letter(c: char): bool = c in 'A'..'Z'
proc neighbours(p: Pos): array[4, Pos] =
  [ (p.x, p.y+1), (p.x+1, p.y), (p.x, p.y-1), (p.x-1, p.y) ]

proc parse(): Map =
  var grid: Table[Pos, char]
  var xMax, yMax: int16
  for line in lines("input.txt"):
    xMax = 0
    for ch in line:
      grid[(xMax, yMax)] = ch
      inc xMax
    inc yMax
  result = Map(xMax: xMax, yMax: yMax, grid: grid)

  var cache: Table[string, Pos]
  for i in 0..<xMax:
    for j in 0..<yMax:
      let c1 = result.grid[(i.int16, j.int16)]
      if not c1.letter: continue
      var portalName = ""
      var portalPoint = (0'i16, 0'i16)
      var ok = false
      block check:
        let c2 = result.grid.getOrDefault((i.int16+1, j.int16), ' ')
        if c2.letter:
          portalName = $c1 & $c2
          if result.grid.getOrDefault((i.int16+2, j.int16), ' ') == Free:
            portalPoint = (i.int16+2, j.int16); ok = true; break check
          if result.grid.getOrDefault((i.int16-1, j.int16), ' ') == Free:
            portalPoint = (i.int16-1, j.int16); ok = true; break check
        let c3 = result.grid.getOrDefault((i.int16, j.int16+1), ' ')
        if c3.letter:
          portalName = $c1 & $c3
          if result.grid.getOrDefault((i.int16, j.int16+2), ' ') == Free:
            portalPoint = (i.int16, j.int16+2); ok = true; break check
          if result.grid.getOrDefault((i.int16, j.int16-1), ' ') == Free:
            portalPoint = (i.int16, j.int16-1); ok = true; break check
      if not ok: continue
      result.portalName[portalPoint] = portalName
      if portalName == "AA":
        result.aa = portalPoint
        result.isOuter[portalPoint] = true
        continue
      if portalName == "ZZ":
        result.zz = portalPoint
        result.isOuter[portalPoint] = true
        continue
      if cache.hasKey(portalName):
        let target = cache[portalName]
        result.teleport[portalPoint] = target
        result.teleport[target] = portalPoint
      else:
        cache[portalName] = portalPoint
      result.isOuter[portalPoint] = j == 0 or i == 0 or i == xMax-2 or j == yMax-2

proc bfsNested(m: Map): int =
  type Status = tuple[p: Pos; depth: int]
  var discovered: Table[Status, bool]
  var todo = initDeque[Status]()
  let root = (m.aa, 0)
  discovered[root] = true
  todo.addLast(root)
  var steps = 0
  while todo.len > 0:
    for levelSize in countdown(todo.len, 1):
      let curr = todo.popFirst()
      for n in neighbours(curr.p):
        let dest = m.grid.getOrDefault(n, Wall)
        if dest == Wall: continue
        if dest == Free:
          let target = (n, curr.depth)
          if not discovered.getOrDefault(target, false):
            discovered[target] = true
            todo.addLast(target)
        elif dest.letter:
          var target: Status
          let outer = m.isOuter.getOrDefault(curr.p, false)
          if not outer:
            target = (m.teleport.getOrDefault(curr.p, curr.p), curr.depth + 1)
          else:
            let name = m.portalName.getOrDefault(curr.p, "")
            if curr.depth == 0:
              if name == "ZZ": return steps
              continue
            if name == "AA" or name == "ZZ": continue
            target = (m.teleport.getOrDefault(curr.p, curr.p), curr.depth - 1)
          if not discovered.getOrDefault(target, false):
            discovered[target] = true
            todo.addLast(target)
    inc steps
  result = -1

when isMainModule:
  let m = parse()
  echo bfsNested(m)
