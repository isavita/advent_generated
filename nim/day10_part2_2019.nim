
import std/[strutils, math, algorithm, sequtils, sets, tables]

type
  Point = tuple[x, y: int]

proc `-`(a, b: Point): Point = (x: a.x - b.x, y: a.y - b.y)

proc gcd(a, b: int): int =
  var (a, b) = (abs(a), abs(b))
  if a < b: swap(a, b)
  while b != 0:
    let t = b
    b = a mod b
    a = t
  result = a

proc reduceFraction(p: Point): Point =
  let d = gcd(p.x, p.y)
  if d == 0:
    return p
  result = (x: p.x div d, y: p.y div d)

proc angle(p: Point): float =
  result = arctan2(float(p.x), -float(p.y))
  if result < 0:
    result += 2 * PI

proc main =
  let input = readFile("input.txt").strip.splitLines
  var asteroids: seq[Point]
  for y, line in input:
    for x, ch in line:
      if ch == '#':
        asteroids.add((x, y))

  var maxCount = 0
  var bestStation: Point
  for station in asteroids:
    var directions: HashSet[Point]
    for other in asteroids:
      if other == station: continue
      let diff = other - station
      directions.incl(reduceFraction(diff))
    if directions.len > maxCount:
      maxCount = directions.len
      bestStation = station

  echo maxCount

  var targets: seq[Point]
  for a in asteroids:
    if a != bestStation:
      targets.add(a)

  var groups: Table[float, seq[Point]]
  for t in targets:
    let diff = t - bestStation
    let dir = reduceFraction(diff)
    let theta = angle(dir)
    if not groups.hasKey(theta):
      groups[theta] = newSeq[Point]()
    groups[theta].add(t)

  for theta in groups.keys:
    groups[theta].sort do (a, b: Point) -> int:
      let da = (a.x - bestStation.x)^2 + (a.y - bestStation.y)^2
      let db = (b.x - bestStation.x)^2 + (b.y - bestStation.y)^2
      result = cmp(da, db)

  var angles = toSeq(groups.keys)
  angles.sort()

  var vaporized: seq[Point]
  while vaporized.len < 300:
    for theta in angles:
      if groups[theta].len > 0:
        vaporized.add(groups[theta][0])
        groups[theta].delete(0)

  let asteroid200 = vaporized[199]
  echo asteroid200.x * 100 + asteroid200.y

when isMainModule:
  main()
