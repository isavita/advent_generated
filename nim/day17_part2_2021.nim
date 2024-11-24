
import std/[strutils, sets, parseutils]

proc isMovingAway(xPos, yPos, xVel, yVel, xMin, xMax, yMin, yMax: int): bool =
  (xPos < xMin and xVel < 0) or 
  (xPos > xMax and xVel > 0) or 
  (yPos < yMin and yVel < 0)

proc main() =
  let input = readFile("input.txt").strip()
  let parts = input.split(", ")
  
  var xRange = parts[0][15..^1].split("..")
  var yRange = parts[1][2..^1].split("..")
  
  let xMin = parseInt(xRange[0])
  let xMax = parseInt(xRange[1])
  let yMin = parseInt(yRange[0])
  let yMax = parseInt(yRange[1])
  
  var velocities = initHashSet[tuple[x, y: int]]()
  
  for xVel in -1000..1000:
    for yVel in -1000..1000:
      var xPos = 0
      var yPos = 0
      var curXVel = xVel
      var curYVel = yVel
      var inTargetArea = false
      
      while true:
        xPos += curXVel
        yPos += curYVel
        
        if xPos in xMin..xMax and yPos in yMin..yMax:
          inTargetArea = true
          break
        
        if isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax):
          break
        
        if curXVel > 0:
          dec curXVel
        elif curXVel < 0:
          inc curXVel
        
        dec curYVel
      
      if inTargetArea:
        velocities.incl((xVel, yVel))
  
  echo velocities.len

main()
