
import strutils, sequtils, tables, math, os

proc main() =
  var overlaps = initTable[(int, int), int]()
  let content = readFile("input.txt")

  for lineStr in content.strip.splitLines:
    if lineStr.len == 0: continue

    let parts = lineStr.split(" -> ")
    let startParts = parts[0].split(',')
    let endParts = parts[1].split(',')

    let x1 = parseInt(startParts[0])
    let y1 = parseInt(startParts[1])
    let x2 = parseInt(endParts[0])
    let y2 = parseInt(endParts[1])

    let dx = x2 - x1
    let dy = y2 - y1
    let xStep = cmp(dx, 0)
    let yStep = cmp(dy, 0)
    let steps = max(abs(dx), abs(dy))

    var currX = x1
    var currY = y1
    for i in 0 .. steps:
      let point = (currX, currY)
      overlaps[point] = overlaps.getOrDefault(point) + 1
      currX += xStep
      currY += yStep

  let count = overlaps.values.toSeq.countIt(it > 1)
  echo count

main()
