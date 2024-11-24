
import std/[strutils, sets, tables, deques, os]

type
  Point = tuple[x, y: int]
  DoorMap = Table[Point, HashSet[Point]]

proc move(p: Point, dir: char): Point =
  case dir
  of 'N': (p.x, p.y - 1)
  of 'S': (p.x, p.y + 1)
  of 'E': (p.x + 1, p.y)
  of 'W': (p.x - 1, p.y)
  else: p

proc buildMap(regex: string): DoorMap =
  var 
    dm = initTable[Point, HashSet[Point]]()
    stack: seq[Point] = @[]
    cp: Point = (0, 0)

  for c in regex:
    case c
    of '(':
      stack.add(cp)
    of '|':
      cp = stack[^1]
    of ')':
      cp = stack[^1]
      stack.delete(stack.high)
    else:
      let np = move(cp, c)
      if cp notin dm:
        dm[cp] = initHashSet[Point]()
      dm[cp].incl(np)
      cp = np

  return dm

proc countRooms(dm: DoorMap, minDoors: int): int =
  var 
    visited = initTable[Point, int]()
    queue = initDeque[Point]()
  
  queue.addLast((0, 0))
  visited[(0, 0)] = 0

  while queue.len > 0:
    let p = queue.popFirst()
    if p in dm:
      for np in dm[p]:
        if np notin visited:
          visited[np] = visited[p] + 1
          if visited[np] >= minDoors:
            inc result
          queue.addLast(np)

proc main() =
  let 
    regex = readFile("input.txt").strip()
    dm = buildMap(regex[1..^2])
    rooms = countRooms(dm, 1000)
  
  echo rooms

main()
