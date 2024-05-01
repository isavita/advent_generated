import strutils, sequtils, algorithm, sets

type
  Direction = enum
    U, D, L, R
  Point = tuple[x: int, y: int]

proc parseWirePath(path: string): seq[(Direction, int)] =
  var result: seq[(Direction, int)] = @[]
  for part in path.split(','):
    let direction = case part[0]:
      of 'U': U
      of 'D': D
      of 'L': L
      of 'R': R
      else: raise newException(IOError, "Invalid direction")
    let distance = parseInt(part[1..^1])
    result.add (direction, distance)
  result

proc getWirePoints(path: seq[(Direction, int)]): HashSet[Point] =
  var x, y: int
  var points: HashSet[Point] = initHashSet[Point]()
  points.incl (0, 0)
  for (direction, distance) in path:
    for _ in 1..distance:
      case direction:
      of U: y += 1
      of D: y -= 1
      of L: x -= 1
      of R: x += 1
      points.incl (x, y)
  points

proc findIntersections(points1, points2: HashSet[Point]): seq[Point] =
  var intersections: seq[Point] = @[]
  for point in points1:
    if point in points2 and point != (0, 0):
      intersections.add point
  intersections

proc manhattanDistance(point: Point): int =
  abs(point.x) + abs(point.y)

when isMainModule:
  let input = readFile("input.txt").strip().splitLines()
  let wire1Path = parseWirePath(input[0])
  let wire2Path = parseWirePath(input[1])
  let wire1Points = getWirePoints(wire1Path)
  let wire2Points = getWirePoints(wire2Path)
  let intersections = findIntersections(wire1Points, wire2Points)
  let distances = intersections.map(manhattanDistance)
  echo "Minimum distance: ", distances.min