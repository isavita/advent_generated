
import strutils, math

type
  Coord = object
    x, y, z: float
  Point = object
    pos, vel: Coord

proc parseInput(lines: seq[string]): seq[Point] =
  result = @[]
  for line in lines:
    let parts = line.split(" @ ")
    let posParts = parts[0].strip.split(", ")
    let velParts = parts[1].strip.split(", ")
    
    let pos = Coord(x: parseFloat(posParts[0].strip), y: parseFloat(posParts[1].strip), z: parseFloat(posParts[2].strip))
    let vel = Coord(x: parseFloat(velParts[0].strip), y: parseFloat(velParts[1].strip), z: parseFloat(velParts[2].strip))
    result.add(Point(pos: pos, vel: vel))

proc isIntersecting2D(p1, p2: Point): tuple[intersects: bool, coord: Coord, t1, t2: float] =
  let det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
  if abs(det) < 1e-9: # Use tolerance for float comparison
    return (false, Coord(x: 0.0, y: 0.0, z: 0.0), 0.0, 0.0)
  
  let t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
  let t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
  
  let intersectionCoord = Coord(x: p1.pos.x + p1.vel.x * t1, y: p1.pos.y + p1.vel.y * t1, z: 0.0)
  
  return (true, intersectionCoord, t1, t2)

proc solve(lines: seq[string], minVal, maxVal: float): int =
  let points = parseInput(lines)
  var count = 0
  for i in 0 ..< points.len:
    for j in 0 ..< i:
      let (intersects, coord, t1, t2) = isIntersecting2D(points[i], points[j])
      let isInBound = coord.x >= minVal and coord.x <= maxVal and coord.y >= minVal and coord.y <= maxVal
      if intersects and isInBound and t1 >= 0.0 and t2 >= 0.0:
        count += 1
  return count

proc main() =
  const
    minCoord = 200000000000000.0
    maxCoord = 400000000000000.0
    inputFile = "input.txt"
    
  let lines = readFile(inputFile).strip.splitLines()
  let result = solve(lines, minCoord, maxCoord)
  echo result

when isMainModule:
  main()
