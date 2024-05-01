import strutils, sequtils, math, tables

type Position = object
  x, y: int

proc `+`(a, b: Position): Position =
  Position(x: a.x + b.x, y: a.y + b.y)

proc `-`(a: Position): Position =
  Position(x: -a.x, y: -a.y)

proc abs(a: int): int =
  if a < 0: -a else: a

proc firstRevisitedDistance(instructions: seq[string]): int =
  var pos = Position(x: 0, y: 0)
  var visited = initTable[Position, bool]()
  let directions = @[Position(x: 0, y: 1), Position(x: 1, y: 0), Position(x: 0, y: -1), Position(x: -1, y: 0)]
  var dirIndex = 0

  visited[pos] = true

  for instruction in instructions:
    let turn = instruction[0]
    let blocks = parseInt(instruction[1..^1])

    if turn == 'R':
      dirIndex = (dirIndex + 1) mod 4
    else:
      dirIndex = (dirIndex - 1 + 4) mod 4

    for _ in 1..blocks:
      pos = pos + directions[dirIndex]
      if pos in visited:
        return abs(pos.x) + abs(pos.y)
      visited[pos] = true

  return -1

let file = open("input.txt")
let instructions = file.readLine().split(", ")
echo firstRevisitedDistance(instructions)