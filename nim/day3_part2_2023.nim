
import strutils, tables, sets, os

type
  Part = object
    xmin, xmax, y, n: int

const neighbors_8 = [
  (x: 0, y: 1), (x: 0, y: -1), (x: 1, y: 0), (x: -1, y: 0),
  (x: -1, y: -1), (x: -1, y: 1), (x: 1, y: -1), (x: 1, y: 1)
]

proc main() =
  let inputData = readFile("input.txt").strip()

  var grid: Table[(int, int), char]
  var parts: seq[Part]
  var parsingNumber = false
  var currentPart: Part # Will be initialized when parsingNumber becomes true

  let lines = inputData.splitLines()
  for y, line in lines.pairs:
    if parsingNumber:
      parts.add(currentPart)
      parsingNumber = false
    for x, c in line.pairs:
      grid[(x, y)] = c
      if c.isDigit():
        let digit = int(c) - int('0')
        if not parsingNumber:
          currentPart = Part(xmin: x, xmax: x, y: y, n: digit)
          parsingNumber = true
        else:
          currentPart.n = currentPart.n * 10 + digit
          currentPart.xmax = x
      elif parsingNumber:
        parts.add(currentPart)
        parsingNumber = false

  # Handle number potentially ending at the very end of the input
  if parsingNumber:
    parts.add(currentPart)

  var partsGrid: Table[(int, int), int]
  for i, p in parts.pairs:
    for x in p.xmin .. p.xmax:
      partsGrid[(x, p.y)] = i

  var sumVal = 0
  for pos, c in grid.pairs:
    if c == '*':
      var neighborParts: HashSet[int]
      for neighbor in neighbors_8:
        let neighborPos = (pos[0] + neighbor.x, pos[1] + neighbor.y)
        if partsGrid.contains(neighborPos):
          neighborParts.incl(partsGrid[neighborPos])

      if neighborParts.len == 2:
        var prod = 1
        for i in neighborParts:
          prod *= parts[i].n
        sumVal += prod

  echo sumVal

main()
