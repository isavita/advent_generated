
import os
import strutils
import sets
import math

# Define a type for coordinates for clarity
type Coord = tuple[x, y: int]

# Constants
const SandSource: Coord = (x: 500, y: 0)

# Function to parse a coordinate string "x,y"
proc parseCoord(s: string): Coord =
  let parts = s.strip().split(',')
  if parts.len != 2:
    raise newException(ValueError, "Invalid coordinate format: " & s)
  return (x: parseInt(parts[0]), y: parseInt(parts[1]))

# Function to simulate sand falling
# Returns a tuple: (sandCountPart1, sandCountPart2)
proc simulateSand(obstacles: var HashSet[Coord], maxY: int): (int, int) =
  var initialObstacles = obstacles # Keep a copy for Part 2 reset
  var sandCountPart1 = 0
  var sandCountPart2 = 0
  var part1Done = false
  let floorY = maxY + 2

  # --- Part 1 Simulation ---
  block part1Loop:
    while true:
      var sandPos = SandSource

      # Simulate one unit of sand falling
      while true:
        # Check abyss condition for Part 1
        if sandPos.y > maxY:
          part1Done = true
          break part1Loop # Sand falls into the abyss

        # Try moving down
        var nextPos = (x: sandPos.x, y: sandPos.y + 1)
        if nextPos notin obstacles:
          sandPos = nextPos
          continue

        # Try moving down-left
        nextPos = (x: sandPos.x - 1, y: sandPos.y + 1)
        if nextPos notin obstacles:
          sandPos = nextPos
          continue

        # Try moving down-right
        nextPos = (x: sandPos.x + 1, y: sandPos.y + 1)
        if nextPos notin obstacles:
          sandPos = nextPos
          continue

        # Sand comes to rest
        obstacles.incl(sandPos)
        sandCountPart1 += 1
        break # Start next sand unit

  # Reset obstacles and start Part 2 simulation
  obstacles = initialObstacles
  sandCountPart2 = 0

  # --- Part 2 Simulation ---
  while true:
    var sandPos = SandSource

    # Check if source is already blocked (Part 2 end condition)
    if SandSource in obstacles:
      break

    # Simulate one unit of sand falling
    while true:
      # Check potential positions including floor
      let down = (x: sandPos.x, y: sandPos.y + 1)
      let downLeft = (x: sandPos.x - 1, y: sandPos.y + 1)
      let downRight = (x: sandPos.x + 1, y: sandPos.y + 1)

      # Check floor collision
      if down.y >= floorY: # Came to rest on the floor
         obstacles.incl(sandPos)
         sandCountPart2 += 1
         break # Sand rests, start next unit

      # Try moving down
      if down notin obstacles:
        sandPos = down
        continue

      # Try moving down-left
      if downLeft notin obstacles:
        sandPos = downLeft
        continue

      # Try moving down-right
      if downRight notin obstacles:
        sandPos = downRight
        continue

      # Sand comes to rest on rock or other sand
      obstacles.incl(sandPos)
      sandCountPart2 += 1
      break # Start next sand unit

  return (sandCountPart1, sandCountPart2)

# Main execution block
proc main() =
  let filename = "input.txt"
  if not fileExists(filename):
    echo "Error: Input file '", filename, "' not found."
    quit(1)

  var obstacles = initHashSet[Coord]()
  var maxY = 0

  # Read and parse the input file
  for line in lines(filename):
    if line.len == 0: continue # Skip empty lines

    let points = line.split(" -> ")
    if points.len < 2: continue # Need at least two points to form a line

    var prevCoord = parseCoord(points[0])
    maxY = max(maxY, prevCoord.y)

    for i in 1 ..< points.len:
      let currentCoord = parseCoord(points[i])
      maxY = max(maxY, currentCoord.y)

      # Add rock points between prevCoord and currentCoord
      if prevCoord.x == currentCoord.x: # Vertical line
        let yStart = min(prevCoord.y, currentCoord.y)
        let yEnd = max(prevCoord.y, currentCoord.y)
        for y in yStart .. yEnd:
          obstacles.incl((x: prevCoord.x, y: y))
      elif prevCoord.y == currentCoord.y: # Horizontal line
        let xStart = min(prevCoord.x, currentCoord.x)
        let xEnd = max(prevCoord.x, currentCoord.x)
        for x in xStart .. xEnd:
          obstacles.incl((x: x, y: prevCoord.y))
      else:
        # Diagonal lines are not expected based on problem description,
        # but handle just in case or raise an error.
        echo "Warning: Diagonal line detected between ", prevCoord, " and ", currentCoord, ". Ignoring."
        # Or: raise newException(ValueError, "Diagonal lines not supported")

      prevCoord = currentCoord

  # Run the simulation
  let (resultPart1, resultPart2) = simulateSand(obstacles, maxY)

  # Print results
  echo "Part 1: ", resultPart1
  echo "Part 2: ", resultPart2

# Execute the main procedure
when isMainModule:
  main()
