
fs = require 'fs'

# Shapes defined as [ [x, y] ] coordinates relative to the bottom-left of the shape.
shapes = [
  [[0, 0], [1, 0], [2, 0], [3, 0]],       # Horizontal line
  [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]], # Plus
  [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]], # Reverse L
  [[0, 0], [0, 1], [0, 2], [0, 3]],       # Vertical line
  [[0, 0], [1, 0], [0, 1], [1, 1]]        # Square
]

# Function to check if a rock placement is valid.
isValidPlacement = (chamber, rock, x, y) ->
  for point in rock
    nx = x + point[0]
    ny = y + point[1]
    if nx < 0 or nx >= 7 or ny < 0 or chamber[ny]?[nx]
      return false
  return true

# Function to add a rock to the chamber.  Modifies the chamber in place.
addRockToChamber = (chamber, rock, x, y) ->
  for point in rock
    nx = x + point[0]
    ny = y + point[1]
    while chamber.length <= ny
      chamber.push []
    chamber[ny][nx] = true


solve = (jetPattern, numRocks) ->
    chamber = []  # Represent the chamber as an array of rows. True indicates a filled cell.
    jetIndex = 0
    height = 0
    rockCount = 0
    seenStates = new Map()
    addedHeight = 0
    
    while rockCount < numRocks
      rockShapeIndex = rockCount % shapes.length
      rock = shapes[rockShapeIndex]
      x = 2
      y = height + 3

      # Check for cycle detection
      stateKey = "#{rockShapeIndex},#{jetIndex}"
      if seenStates.has(stateKey) and rockCount >= 2022
        prevRockCount = seenStates.get(stateKey)[0]
        prevHeight = seenStates.get(stateKey)[1]

        cycleLength = rockCount - prevRockCount
        remainingRocks = numRocks - rockCount
        numCycles = Math.floor(remainingRocks / cycleLength)
        addedHeight += numCycles * (height - prevHeight)
        rockCount += numCycles * cycleLength
        seenStates.clear() #Clear, or we will find the same state again and get stuck in a loop

      else
          seenStates.set(stateKey, [rockCount,height])


      while true
        # Horizontal movement
        jet = jetPattern[jetIndex]
        jetIndex = (jetIndex + 1) % jetPattern.length
        if jet is '>' and isValidPlacement(chamber, rock, x + 1, y)
          x += 1
        else if jet is '<' and isValidPlacement(chamber, rock, x - 1, y)
          x -= 1

        # Downward movement
        if isValidPlacement(chamber, rock, x, y - 1)
          y -= 1
        else
          addRockToChamber(chamber, rock, x, y)
          height = Math.max(height, y + rock.reduce(((max, p) -> Math.max(max, p[1] + 1)), 0))
          break
      rockCount +=1

    return height + addedHeight

# Read the jet pattern from input.txt.
jetPattern = fs.readFileSync('input.txt', 'utf8').trim()

# --- Part One ---
part1Result = solve(jetPattern, 2022)
console.log "Part 1: #{part1Result}"

# --- Part Two ---
part2Result = solve(jetPattern, 1000000000000)
console.log "Part 2: #{part2Result}"

