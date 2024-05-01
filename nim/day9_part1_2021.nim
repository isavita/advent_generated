import strutils, sequtils

var heightmap: seq[seq[int]]

proc isLowPoint(heightmap: seq[seq[int]], x, y: int): bool =
  let height = heightmap[y][x]
  if x > 0 and heightmap[y][x-1] <= height: return false
  if x < heightmap[y].len - 1 and heightmap[y][x+1] <= height: return false
  if y > 0 and heightmap[y-1][x] <= height: return false
  if y < heightmap.len - 1 and heightmap[y+1][x] <= height: return false
  return true

for line in "input.txt".lines:
  var row: seq[int]
  for char in line:
    row.add parseInt($char)
  heightmap.add row

var totalRiskLevel = 0
for y, row in heightmap.pairs:
  for x, height in row.pairs:
    if isLowPoint(heightmap, x, y):
      totalRiskLevel += 1 + height

echo totalRiskLevel