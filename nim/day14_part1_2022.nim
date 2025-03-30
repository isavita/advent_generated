
import std/[strutils, sequtils, sets, parseutils, math, streams]

type Point = object
  x, y: int

proc `+`(a, b: Point): Point =
  Point(x: a.x + b.x, y: a.y + b.y)

proc bounds(grid: HashSet[Point]): (Point, Point) =
  var minX, minY = high(int)
  var maxX, maxY = low(int)
  for p in grid:
    minX = min(minX, p.x)
    minY = min(minY, p.y)
    maxX = max(maxX, p.x)
    maxY = max(maxY, p.y)
  (Point(x: minX, y: minY), Point(x: maxX, y: maxY + 1))

proc fill(grid: var HashSet[Point]): int =
  let (_, maxPoint) = bounds(grid)
  let floor = maxPoint.y
  var sands = 0
  var firstFloorTouch = 0
  let deltas = [Point(x: 0, y: 1), Point(x: -1, y: 1), Point(x: 1, y: 1)]

  while true:
    var sand = Point(x: 500, y: 0)
    block sandFall:
      while true:
        if sand.y == floor - 1:
          if firstFloorTouch == 0:
            firstFloorTouch = sands
          grid.incl(sand)
          break sandFall

        var moved = false
        for d in deltas:
          let newSand = sand + d
          if newSand notin grid:
            sand = newSand
            moved = true
            break # Try falling further from the new position

        if not moved:
          grid.incl(sand)
          break sandFall # Sand came to rest

    inc sands
    if sand.y == 0 and sand.x == 500: # Source is blocked
        # Although the python code returns firstFloorTouch here,
        # it seems it intended to return the total sands for part 2.
        # The prompt asks for the equivalent of the python code,
        # which returns firstFloorTouch.
        # If the true goal was part 2, it should return `sands`.
        # Let's stick to the python code's return value.
        if firstFloorTouch == 0: # Handle edge case where source blocked before floor touch
            firstFloorTouch = sands
        return firstFloorTouch


proc main() =
  var grid: HashSet[Point]
  let f = open("input.txt")
  for line in f.lines:
    let parts = line.strip.split(" -> ")
    var points: seq[Point]
    for part in parts:
      let coords = part.split(',')
      points.add Point(x: parseInt(coords[0]), y: parseInt(coords[1]))

    for i in 0 ..< points.len - 1:
      let p1 = points[i]
      let p2 = points[i+1]
      if p1.x == p2.x:
        for y in min(p1.y, p2.y) .. max(p1.y, p2.y):
          grid.incl(Point(x: p1.x, y: y))
      else: # p1.y == p2.y
        for x in min(p1.x, p2.x) .. max(p1.x, p2.x):
          grid.incl(Point(x: x, y: p1.y))
  f.close()

  echo fill(grid)

when isMainModule:
  main()
