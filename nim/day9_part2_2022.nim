import strutils, sequtils, sets

type
  Point = object
    x, y: int

proc moveTail(head, tail: Point): Point =
  var newTail = tail
  let dx = head.x - tail.x
  let dy = head.y - tail.y
  if abs(dx) > 1 or abs(dy) > 1:
    if dx != 0: newTail.x += dx div abs(dx)
    if dy != 0: newTail.y += dy div abs(dy)
  newTail

proc main() =
  let file = readFile("input.txt")
  var knots: seq[Point] = newSeq[Point](10)
  var visited: HashSet[Point] = initHashSet[Point]()
  visited.incl(knots[^1])

  for line in file.splitLines():
    let parts = line.split()
    let dir = parts[0]
    let steps = parseInt(parts[1])

    for _ in 0..<steps:
      case dir
      of "U": knots[0].y += 1
      of "D": knots[0].y -= 1
      of "L": knots[0].x -= 1
      of "R": knots[0].x += 1
      else: discard

      for i in 1..<knots.len:
        knots[i] = moveTail(knots[i-1], knots[i])

      visited.incl(knots[^1])

  echo visited.len

when isMainModule:
  main()