
import strutils, sequtils, math

type Coord = object
  x, y: int

proc add*(c1, c2: Coord): Coord =
  Coord(x: c1.x + c2.x, y: c1.y + c2.y)

proc multiplyByScalar*(c: Coord; s: int): Coord =
  Coord(x: c.x * s, y: c.y * s)

const
  North = Coord(x: 0, y: -1)
  West = Coord(x: -1, y: 0)
  South = Coord(x: 0, y: 1)
  East = Coord(x: 1, y: 0)

proc abs(x: int): int =
  if x < 0: -x else: x

proc parseInput(input: seq[string]): seq[Coord] =
  var current = Coord(x: 0, y: 0)
  var vertices = @[current]

  for line in input:
    let parts = line.splitWhitespace()
    let dirInput = parts[0][0]
    let length = parseInt(parts[1])

    var dir: Coord
    case dirInput:
    of 'U': dir = North
    of 'L': dir = West
    of 'D': dir = South
    of 'R': dir = East
    else: raise newException(ValueError, "Invalid direction")

    current = current.add(dir.multiplyByScalar(length))
    vertices.add(current)

  vertices

proc shoelace(vertices: seq[Coord]): int =
  let n = vertices.len
  var area = 0

  for i in 0..<n:
    let next = (i + 1) mod n
    area += vertices[i].x * vertices[next].y
    area -= vertices[i].y * vertices[next].x

  abs(area) div 2

proc perimeter(vertices: seq[Coord]): int =
  let n = vertices.len
  var perim = 0

  for i in 0..<n:
    let next = (i + 1) mod n
    perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)

  perim

proc calculatePolygonArea(vertices: seq[Coord]): int =
  shoelace(vertices) + perimeter(vertices) div 2 + 1

proc solve(input: seq[string]): int =
  let vertices = parseInput(input)
  calculatePolygonArea(vertices)

proc main() =
  let input = readFile("input.txt").splitLines()
  echo solve(input)

main()
