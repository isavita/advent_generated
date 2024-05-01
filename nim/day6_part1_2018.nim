import strutils, sequtils, math

type Point = object
  x, y: int

var file = open("input.txt")
var points: seq[Point]
var maxX, maxY: int

for line in file.lines:
  let coords = line.split(", ")
  let x = parseInt(coords[0])
  let y = parseInt(coords[1])
  if x > maxX:
    maxX = x
  if y > maxY:
    maxY = y
  points.add(Point(x: x, y: y))

file.close()

var grid: seq[seq[int]]
newSeq(grid, maxX + 2)
for i in 0..grid.len - 1:
  newSeq(grid[i], maxY + 2)

var areas: seq[int]
newSeq(areas, points.len)
var infinite: seq[bool]
newSeq(infinite, points.len)

for i in 0..grid.len - 1:
  for j in 0..grid[i].len - 1:
    var minDist = maxX + maxY
    for k, point in points:
      let dist = abs(point.x - i) + abs(point.y - j)
      if dist < minDist:
        minDist = dist
        grid[i][j] = k
      elif dist == minDist:
        grid[i][j] = -1
    if grid[i][j] != -1:
      if i == 0 or j == 0 or i == maxX + 1 or j == maxY + 1:
        infinite[grid[i][j]] = true
      areas[grid[i][j]] += 1

var maxArea = 0
for i, area in areas:
  if not infinite[i] and area > maxArea:
    maxArea = area

echo maxArea