
import std/[strutils, sets, sequtils, strformat]

proc main() =
  var points = initHashSet[(int, int)]()
  var folds: seq[(int, int)] = @[]
  var readingPoints = true

  for line in lines("input.txt"):
    if line == "":
      readingPoints = false
      continue

    if readingPoints:
      let parts = line.split(",")
      points.incl((parts[0].parseInt, parts[1].parseInt))
    else:
      let parts = line.split("=")
      let val = parts[1].parseInt
      if "x" in parts[0]:
        folds.add((val, 0))
      else:
        folds.add((0, val))

  for i, fold in folds:
    var newPoints = initHashSet[(int, int)]()
    for point in points:
      var newPoint = point
      if fold[0] != 0 and point[0] > fold[0]:
        newPoint = (fold[0] - (point[0] - fold[0]), point[1])
      elif fold[1] != 0 and point[1] > fold[1]:
        newPoint = (point[0], fold[1] - (point[1] - fold[1]))
      newPoints.incl(newPoint)
    
    points = newPoints
    if i == 0:
      echo &"Number of dots visible after first fold: {len(points)}"

  let maxX = points.mapIt(it[0]).max
  let maxY = points.mapIt(it[1]).max

  var grid = newSeqWith(maxY + 1, newSeqWith(maxX + 1, ' '))
  
  for point in points:
    grid[point[1]][point[0]] = '#'

  for row in grid:
    echo row.join("")

main()
