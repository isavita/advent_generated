import os
import strutils
import sequtils
import strformat
import tables

type
  Point = tuple[x, y: int]
  Room = object
    visited: bool
    distance: int

proc maxDistance(rooms: Table[Point, Room]): int =
  var maxDist = 0
  for room in rooms.values:
    if room.distance > maxDist:
      maxDist = room.distance
  maxDist

proc solve(filename: string) =
  let directions = readFile(filename).strip()[1..^2]
  var rooms = initTable[Point, Room]()
  var stack: seq[Point]
  var currentPos: Point = (0, 0)
  rooms[currentPos] = Room(visited: true, distance: 0)

  for dir in directions:
    case dir
    of '(':
      stack.add(currentPos)
    of ')':
      currentPos = stack.pop()
    of '|':
      currentPos = stack[^1]
    of 'N':
      currentPos.y.inc()
      if not rooms.hasKey(currentPos):
        rooms[currentPos] = Room(visited: true, distance: rooms[(currentPos.x, currentPos.y - 1)].distance + 1)
    of 'S':
      currentPos.y.dec()
      if not rooms.hasKey(currentPos):
        rooms[currentPos] = Room(visited: true, distance: rooms[(currentPos.x, currentPos.y + 1)].distance + 1)
    of 'E':
      currentPos.x.inc()
      if not rooms.hasKey(currentPos):
        rooms[currentPos] = Room(visited: true, distance: rooms[(currentPos.x - 1, currentPos.y)].distance + 1)
    of 'W':
      currentPos.x.dec()
      if not rooms.hasKey(currentPos):
        rooms[currentPos] = Room(visited: true, distance: rooms[(currentPos.x + 1, currentPos.y)].distance + 1)
    else:
      discard

  echo fmt"Furthest room requires passing {maxDistance(rooms)} doors"

solve("input.txt")