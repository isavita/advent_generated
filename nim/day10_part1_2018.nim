
import strutils, sequtils, math, algorithm

type
  Point = object
    x, y: int
    vx, vy: int

proc main() =
  var points: seq[Point] = @[]
  for line in lines("input.txt"):
    let parts = line.split("> ")
    let posPart = parts[0].split("<")[1].split(", ")
    let velPart = parts[1].split("<")[1].split(">")[0].split(", ")
    let x = parseInt(posPart[0].strip())
    let y = parseInt(posPart[1].strip())
    let vx = parseInt(velPart[0].strip())
    let vy = parseInt(velPart[1].strip())
    points.add(Point(x: x, y: y, vx: vx, vy: vy))

  var minArea = int.high
  var bestTime = 0
  var bestPositions: seq[Point] = @[]

  for time in 0..100000:
    var minX = int.high
    var minY = int.high
    var maxX = int.low
    var maxY = int.low
    var currentPoints: seq[Point] = @[]

    for p in points:
      let x = p.x + time * p.vx
      let y = p.y + time * p.vy
      minX = min(minX, x)
      minY = min(minY, y)
      maxX = max(maxX, x)
      maxY = max(maxY, y)
      currentPoints.add(Point(x: x, y: y, vx: p.vx, vy: p.vy))

    let area = (maxX - minX) * (maxY - minY)
    if area < minArea:
      minArea = area
      bestTime = time
      bestPositions = currentPoints
    else:
      # Once the area starts increasing, we break out
      break

  # Now, we have the best time, so we generate the grid for bestPositions
  let minX = min(bestPositions.mapIt(it.x))
  let minY = min(bestPositions.mapIt(it.y))
  let maxX = max(bestPositions.mapIt(it.x))
  let maxY = max(bestPositions.mapIt(it.y))

  var grid = newSeqWith(maxY - minY + 1, newSeq[char](maxX - minX + 1))
  for i in 0..<grid.len:
    for j in 0..<grid[i].len:
      grid[i][j] = '.'

  for p in bestPositions:
    let x = p.x - minX
    let y = p.y - minY
    grid[y][x] = '#'

  for row in grid:
    echo row.join("")

main()
