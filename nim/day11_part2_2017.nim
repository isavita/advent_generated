import strutils, sequtils

type Direction = enum
  n, ne, se, s, sw, nw

proc move(x, y: var int, direction: Direction) =
  case direction
  of n: y.inc
  of ne: x.inc; y.inc
  of se: x.inc; y.dec
  of s: y.dec
  of sw: x.dec; y.dec
  of nw: x.dec; y.inc

proc distance(x, y: int): int =
  let dx = abs(x)
  let dy = abs(y)
  dx + max(0, (dy - dx) div 2)

proc main() =
  let input = readFile("input.txt").strip().split(',')
  var x, y = 0
  var maxDistance = 0
  for direction in input:
    case direction.strip():
    of "n": move(x, y, n)
    of "ne": move(x, y, ne)
    of "se": move(x, y, se)
    of "s": move(x, y, s)
    of "sw": move(x, y, sw)
    of "nw": move(x, y, nw)
    let distance = distance(x, y)
    maxDistance = max(maxDistance, distance)

  echo "Part 1: ", distance(x, y)
  echo "Part 2: ", maxDistance

when isMainModule:
  main()