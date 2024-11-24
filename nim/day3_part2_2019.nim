
import strutils, sequtils, tables, os

type
  Point = tuple[x, y: int]

proc getPointsWithSteps(path: string): Table[Point, int] =
  result = initTable[Point, int]()
  var current = (x: 0, y: 0)
  var steps = 0
  for move in path.split(","):
    let dir = move[0]
    let dist = parseInt(move[1..^1])
    for _ in 0..<dist:
      inc steps
      case dir
      of 'U': inc current.y
      of 'D': dec current.y
      of 'L': dec current.x
      of 'R': inc current.x
      else: discard
      if not result.hasKey(current):
        result[current] = steps

let data = readFile("input.txt")
let lines = data.splitLines()
let wire1 = getPointsWithSteps(lines[0])
let wire2 = getPointsWithSteps(lines[1])

var minSteps = int.high
for p in wire1.keys:
  if wire2.hasKey(p):
    let totalSteps = wire1[p] + wire2[p]
    if totalSteps < minSteps:
      minSteps = totalSteps

echo minSteps
