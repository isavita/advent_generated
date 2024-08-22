import strutils, sequtils, math

type Direction = enum
  North, East, South, West

proc move(direction: Direction, value: int, east: var int, north: var int) =
  case direction
  of North: north += value
  of South: north -= value
  of East: east += value
  of West: east -= value

proc turn(current: Direction, degrees: int, right: bool): Direction =
  var newDir = ord(current)
  if right:
    newDir = (newDir + degrees div 90) mod 4
  else:
    newDir = (newDir - degrees div 90 + 4) mod 4
  Direction(newDir)

proc main() =
  let file = readFile("input.txt")
  let instructions = file.splitLines()

  var east, north = 0
  var facing = Direction.East

  for instruction in instructions:
    let action = instruction[0]
    let value = instruction[1..^1].parseInt()

    case action
    of 'N': move(Direction.North, value, east, north)
    of 'S': move(Direction.South, value, east, north)
    of 'E': move(Direction.East, value, east, north)
    of 'W': move(Direction.West, value, east, north)
    of 'L': facing = turn(facing, value, false)
    of 'R': facing = turn(facing, value, true)
    of 'F': move(facing, value, east, north)
    else: discard  # Handle other cases

  let manhattanDistance = abs(east) + abs(north)
  echo "Manhattan distance: ", manhattanDistance

main()