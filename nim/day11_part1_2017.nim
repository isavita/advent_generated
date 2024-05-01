import strutils

type Direction = enum
  n, ne, se, s, sw, nw

proc move(x, y: var int, dir: Direction) =
  case dir
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
  let input = readFile("input.txt").strip(chars={' '}).split(',')
  var x, y = 0
  for dir in input:
    case dir
    of "n": move(x, y, n)
    of "ne": move(x, y, ne)
    of "se": move(x, y, se)
    of "s": move(x, y, s)
    of "sw": move(x, y, sw)
    of "nw": move(x, y, nw)
    else: raise newException(IOError, "Invalid direction")
  echo "Minimum steps: ", distance(x, y)

when isMainModule:
  main()