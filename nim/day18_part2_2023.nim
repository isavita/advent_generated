
import strutils, math, sequtils, parseutils

type Coord = object
  x, y: int64

proc `+`(a, b: Coord): Coord =
  Coord(x: a.x + b.x, y: a.y + b.y)

proc `*`(c: Coord, s: int64): Coord =
  Coord(x: c.x * s, y: c.y * s)

const
  north = Coord(x: 0, y: -1)
  west = Coord(x: -1, y: 0)
  south = Coord(x: 0, y: 1)
  east = Coord(x: 1, y: 0)

proc parseInputAndPerimeter(inputLines: seq[string]): (seq[Coord], int64) =
  var current = Coord(x: 0, y: 0)
  var vertices: seq[Coord] = @[current]
  var totalPerimeter: int64 = 0

  for line in inputLines:
    let parts = line.splitWhitespace()
    let color = parts[2] # e.g., "(#70c710)"

    let dirInput = color[7]        # Index 7 is the direction digit
    let lengthStr = color[2..6]    # Indices 2 through 6 are the hex length
    let length = parseHexInt(lengthStr).int64

    totalPerimeter += length

    var direction: Coord
    case dirInput
    of '3': direction = north
    of '2': direction = west
    of '1': direction = south
    of '0': direction = east
    else: discard # Assume valid input

    current = current + direction * length
    vertices.add(current)

  return (vertices, totalPerimeter)

proc shoelace(vertices: seq[Coord]): int64 =
  let n = vertices.len
  var area: int64 = 0

  for i in 0 ..< n:
    let nextIdx = (i + 1) mod n
    area += vertices[i].x * vertices[nextIdx].y
    area -= vertices[i].y * vertices[nextIdx].x

  result = abs(area) div 2

proc calculatePolygonArea(vertices: seq[Coord], perimeter: int64): int64 =
  result = shoelace(vertices) + perimeter div 2 + 1

proc solve(inputLines: seq[string]): int64 =
  let (vertices, perimeter) = parseInputAndPerimeter(inputLines)
  result = calculatePolygonArea(vertices, perimeter)

when isMainModule:
  let inputLines = readFile("input.txt").strip.splitLines()
  echo solve(inputLines)
