
import os, strutils, sequtils

proc readInput(): (int, int, int, int) =
  let lines = readFile("input.txt").splitLines()
  let parts = lines[0].split(", ")
  let xRange = parts[0][15..^1].split("..").mapIt(parseInt(it))
  let yRange = parts[1][2..^1].split("..").mapIt(parseInt(it))
  return (xRange[0], xRange[1], yRange[0], yRange[1])

proc isMovingAway(xPos, yPos, xVel, yVel, xMin, xMax, yMin, yMax: int): bool =
  (xPos < xMin and xVel < 0) or (xPos > xMax and xVel > 0) or (yPos < yMin and yVel < 0)

proc main() =
  let (xMin, xMax, yMin, yMax) = readInput()
  var maxY = -1 shl 30

  for xVel in -1000..1000:
    for yVel in -1000..1000:
      var (xPos, yPos) = (0, 0)
      var (curXVel, curYVel) = (xVel, yVel)
      var highestY = yPos

      while true:
        xPos += curXVel
        yPos += curYVel

        if xPos >= xMin and xPos <= xMax and yPos >= yMin and yPos <= yMax:
          maxY = max(maxY, highestY)
          break

        if isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax):
          break

        if curXVel > 0: curXVel -= 1
        elif curXVel < 0: curXVel += 1

        curYVel -= 1
        highestY = max(highestY, yPos)

  echo maxY

main()
