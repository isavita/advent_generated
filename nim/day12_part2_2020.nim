
import std/[strutils, math]

type
  Ship = object
    x, y: int
    waypointX, waypointY: int

proc processInstruction(ship: var Ship, action: char, value: int) =
  case action
  of 'N': ship.waypointY += value
  of 'S': ship.waypointY -= value
  of 'E': ship.waypointX += value
  of 'W': ship.waypointX -= value
  of 'L': 
    let degrees = -value
    case (degrees + 360) mod 360
    of 90:  (ship.waypointX, ship.waypointY) = (ship.waypointY, -ship.waypointX)
    of 180: (ship.waypointX, ship.waypointY) = (-ship.waypointX, -ship.waypointY)
    of 270: (ship.waypointX, ship.waypointY) = (-ship.waypointY, ship.waypointX)
    else: discard
  of 'R':
    let degrees = value
    case degrees mod 360
    of 90:  (ship.waypointX, ship.waypointY) = (ship.waypointY, -ship.waypointX)
    of 180: (ship.waypointX, ship.waypointY) = (-ship.waypointX, -ship.waypointY)
    of 270: (ship.waypointX, ship.waypointY) = (-ship.waypointY, ship.waypointX)
    else: discard
  of 'F':
    ship.x += ship.waypointX * value
    ship.y += ship.waypointY * value
  else: discard

proc main() =
  var ship = Ship(x: 0, y: 0, waypointX: 10, waypointY: 1)
  
  for line in lines("input.txt"):
    let action = line[0]
    let value = parseInt(line[1..^1])
    ship.processInstruction(action, value)
  
  echo abs(ship.x) + abs(ship.y)

main()
