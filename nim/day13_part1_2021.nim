
import strutils, sequtils, sets

type Point = tuple[x, y: int]

proc solve(): int =
  let file = open("input.txt")
  defer: file.close()

  var points: HashSet[Point] = initHashSet[Point]()
  var folds: seq[string] = @[]

  var readingPoints = true
  for line in file.lines:
    if line == "":
      readingPoints = false
      continue
    if readingPoints:
      let coords = line.split(",")
      points.incl((parseInt(coords[0]), parseInt(coords[1])))
    else:
      folds.add(line)

  let fold = folds[0].split(" ")[2].split("=")
  let axis = fold[0]
  let value = parseInt(fold[1])

  var newPoints: HashSet[Point] = initHashSet[Point]()
  for point in points:
    let newPoint =
      if axis == "x":
        if point.x > value: (2 * value - point.x, point.y)
        else: point
      else:
        if point.y > value: (point.x, 2 * value - point.y)
        else: point
    newPoints.incl(newPoint)

  return newPoints.len

echo solve()
