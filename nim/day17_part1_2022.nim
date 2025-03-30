
import sets, strutils, sequtils, options, os

type Point = tuple[x, y: int]

const ChamberWidth = 7

proc readInput(filename: string): string =
  readFile(filename).strip()

proc getRockShapes(): seq[seq[Point]] =
  result = @[
    @[ (x:0, y:0), (x:1, y:0), (x:2, y:0), (x:3, y:0) ], # -
    @[ (x:1, y:0), (x:0, y:1), (x:1, y:1), (x:2, y:1), (x:1, y:2) ], # +
    @[ (x:0, y:0), (x:1, y:0), (x:2, y:0), (x:2, y:1), (x:2, y:2) ], # L
    @[ (x:0, y:0), (x:0, y:1), (x:0, y:2), (x:0, y:3) ], # |
    @[ (x:0, y:0), (x:1, y:0), (x:0, y:1), (x:1, y:1) ]  # square
  ]

type Direction = enum dLeft, dRight, dDown

proc tryMove(rock: seq[Point], direction: Direction, chamber: HashSet[Point]): Option[seq[Point]] =
  var movedRock = newSeq[Point](rock.len)
  for i, p in rock:
    var newX = p.x
    var newY = p.y
    case direction:
      of dLeft: newX -= 1
      of dRight: newX += 1
      of dDown: newY -= 1

    if newX < 0 or newX >= ChamberWidth or newY <= 0:
      return none(seq[Point])
    let newP = (x: newX, y: newY)
    if chamber.contains(newP):
      return none(seq[Point])
    movedRock[i] = newP
  return some(movedRock)

proc simulate(jetPattern: string, totalRocks: int): int =
  let rockShapes = getRockShapes()
  var chamber: HashSet[Point]
  # Floor is implicitly y=0 via checks, no need to add points for it

  var highestY = 0
  var jetIndex = 0
  let jetLen = jetPattern.len
  let rockShapesLen = rockShapes.len

  for rockNum in 0..<totalRocks:
    let shape = rockShapes[rockNum mod rockShapesLen]
    let startX = 2
    let startY = highestY + 4
    
    var currentRock = newSeq[Point](shape.len)
    for i, dp in shape:
        currentRock[i] = (x: startX + dp.x, y: startY + dp.y)

    while true:
      # Jet push
      let jetDirChar = jetPattern[jetIndex mod jetLen]
      jetIndex += 1
      
      let jetMoveDir = if jetDirChar == '<': dLeft else: dRight
      let movedJet = tryMove(currentRock, jetMoveDir, chamber)
      if movedJet.isSome:
        currentRock = movedJet.get()

      # Move down
      let movedDown = tryMove(currentRock, dDown, chamber)
      if movedDown.isSome:
        currentRock = movedDown.get()
      else:
        # Rest
        for p in currentRock:
          chamber.incl(p)
          if p.y > highestY:
            highestY = p.y
        break 

  return highestY

proc main() =
  let jetPattern = readInput("input.txt")
  const totalRocks = 2022
  let finalHeight = simulate(jetPattern, totalRocks)
  echo finalHeight

when isMainModule:
  main()
