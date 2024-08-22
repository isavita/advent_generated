import strutils, sequtils, tables

type
  Point = object
    x, y: int
  Line = object
    p1, p2: Point

proc parseLine(s: string): Line =
  let parts = s.split(" -> ")
  let p1 = parts[0].split(',')
  let p2 = parts[1].split(',')
  result = Line(p1: Point(x: parseInt(p1[0]), y: parseInt(p1[1])),
                p2: Point(x: parseInt(p2[0]), y: parseInt(p2[1])))

proc isHorizontalOrVertical(line: Line): bool =
  line.p1.x == line.p2.x or line.p1.y == line.p2.y

proc main() =
  let input = readFile("input.txt").splitLines()
  var points = initTable[Point, int]()
  
  for lineStr in input:
    let line = parseLine(lineStr)
    if not isHorizontalOrVertical(line): continue

    if line.p1.x == line.p2.x:
      let minY = min(line.p1.y, line.p2.y)
      let maxY = max(line.p1.y, line.p2.y)
      for y in minY..maxY:
        let point = Point(x: line.p1.x, y: y)
        if points.hasKey(point):
          points[point] += 1
        else:
          points[point] = 1
    else:
      let minX = min(line.p1.x, line.p2.x)
      let maxX = max(line.p1.x, line.p2.x)
      for x in minX..maxX:
        let point = Point(x: x, y: line.p1.y)
        if points.hasKey(point):
          points[point] += 1
        else:
          points[point] = 1

  var overlapCount = 0
  for count in points.values:
    if count >= 2:
      overlapCount += 1

  echo overlapCount

main()