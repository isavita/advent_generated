import strutils, sequtils, tables

type
  Position = object
    x, y: int

proc moveHead(head: var Position, direction: char) =
  case direction
  of 'R': head.x += 1
  of 'L': head.x -= 1
  of 'U': head.y += 1
  of 'D': head.y -= 1
  else: discard

proc moveTail(tail: var Position, head: Position) =
  if abs(head.x - tail.x) > 1 or abs(head.y - tail.y) > 1:
    if head.x > tail.x: tail.x += 1
    elif head.x < tail.x: tail.x -= 1
    if head.y > tail.y: tail.y += 1
    elif head.y < tail.y: tail.y -= 1

proc simulateRope(filename: string): int =
  let lines = readFile(filename).splitLines()
  var head, tail: Position
  var visited = initTable[Position, bool]()
  visited[tail] = true

  for line in lines:
    let parts = line.split()
    let direction = parts[0][0]
    let steps = parseInt(parts[1])

    for _ in 0..<steps:
      moveHead(head, direction)
      moveTail(tail, head)
      visited[tail] = true

  visited.len

when isMainModule:
  echo simulateRope("input.txt")