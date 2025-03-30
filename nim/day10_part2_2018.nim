
import re, strutils, system

when isMainModule:
  var points: seq[(int, int)] = @[]
  var velocities: seq[(int, int)] = @[]
  let pattern = re("-?\\d+")

  for line in lines("input.txt"):
    var nums: seq[int]
    for match in findAll(line, pattern):
      nums.add parseInt(match)
    if nums.len == 4:
      points.add((nums[0], nums[1]))
      velocities.add((nums[2], nums[3]))

  var time = 0
  while true:
    var minX = high(int)
    var maxX = low(int)
    var minY = high(int)
    var maxY = low(int)

    for p in points:
      minX = min(minX, p[0])
      maxX = max(maxX, p[0])
      minY = min(minY, p[1])
      maxY = max(maxY, p[1])

    if maxX - minX < 100 and maxY - minY < 10:
      break

    for i in 0 ..< points.len:
      let p = points[i]
      let v = velocities[i]
      points[i] = (p[0] + v[0], p[1] + v[1])

    inc time

  echo time
