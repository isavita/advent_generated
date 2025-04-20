
# Day 23: Experimental Emergency Teleportation
# Solution in CoffeeScript

fs = require 'fs'

# --- Data Structures and Helpers ---

# Represents a nanobot
class Nanobot
  constructor: (@x, @y, @z, @r) ->

  # Calculate Manhattan distance to another point (or Nanobot)
  distanceTo: (other) ->
    Math.abs(@x - other.x) + Math.abs(@y - other.y) + Math.abs(@z - other.z)

# Represents a search box for Part 2
class SearchBox
  constructor: (@x, @y, @z, @size, @potentialBots, @distOrigin) ->

  # Priority for the queue (max potential bots, then min size, then min dist)
  # Use negative potential bots for max-heap behavior with a min-heap implementation/sort
  getPriority: -> [-@potentialBots, @size, @distOrigin]

# Simple Priority Queue using Array Sort (sufficient for this problem scale)
class PriorityQueue
  constructor: ->
    @items = []

  push: (item) ->
    @items.push item
    # Keep sorted by priority (descending potentialBots, ascending size, ascending distOrigin)
    @items.sort (a, b) ->
      pa = a.getPriority()
      pb = b.getPriority()
      for i in [0...pa.length]
        diff = pa[i] - pb[i]
        return diff if diff != 0
      return 0

  pop: ->
    @items.shift() # Removes and returns the highest priority item (first)

  peek: ->
    @items[0]

  isEmpty: ->
    @items.length == 0

  getSize: ->
    @items.length


# --- Parsing ---

parseInput = (filename) ->
  data = fs.readFileSync(filename, 'utf8')
  lines = data.trim().split('\n')
  regex = /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/
  nanobots = lines.map (line) ->
    match = line.match(regex)
    if match
      [_, x, y, z, r] = match
      new Nanobot(parseInt(x), parseInt(y), parseInt(z), parseInt(r))
    else
      throw new Error("Invalid input line: #{line}")
  nanobots

# --- Part 1 ---

solvePart1 = (nanobots) ->
  # Find the nanobot with the largest radius
  strongest = nanobots.reduce (maxBot, currentBot) ->
    if currentBot.r > maxBot.r then currentBot else maxBot

  # Count how many nanobots are in range of the strongest one
  inRangeCount = nanobots.filter((bot) -> strongest.distanceTo(bot) <= strongest.r).length
  inRangeCount

# --- Part 2 ---

solvePart2 = (nanobots) ->
  # Calculate initial bounding box
  minX = Math.min(...nanobots.map (b) -> b.x)
  maxX = Math.max(...nanobots.map (b) -> b.x)
  minY = Math.min(...nanobots.map (b) -> b.y)
  maxY = Math.max(...nanobots.map (b) -> b.y)
  minZ = Math.min(...nanobots.map (b) -> b.z)
  maxZ = Math.max(...nanobots.map (b) -> b.z)

  # Determine initial box size (power of 2 large enough to cover the range)
  maxRange = Math.max(maxX - minX, maxY - minY, maxZ - minZ)
  initialSize = 1
  initialSize *= 2 while initialSize < maxRange

  # Center the initial box roughly around the coordinates, starting at a corner
  # Ensure the box covers all bots by starting from the minimum corner
  # Let's start simply from (minX, minY, minZ) with the power-of-2 size
  # A more robust start might be needed if bots are far from origin,
  # but centering near min coords should work.
  # We could also center around 0,0,0 with a large enough size. Let's try that.
  # Start with a large cube centered roughly at 0, potentially large radius.
  initialSize = 1
  maxAbsCoord = 0
  for bot in nanobots
      maxAbsCoord = Math.max maxAbsCoord, Math.abs(bot.x), Math.abs(bot.y), Math.abs(bot.z), bot.r
  initialSize *= 2 while initialSize <= maxAbsCoord * 2 # Ensure it covers +/- maxAbsCoord

  startX = -initialSize / 2
  startY = -initialSize / 2
  startZ = -initialSize / 2

  # Helper: Calculate Manhattan distance from origin
  originDist = (x, y, z) -> Math.abs(x) + Math.abs(y) + Math.abs(z)

  # Helper: Calculate potential bots intersecting a box
  calculatePotentialBots = (boxX, boxY, boxZ, boxSize, bots) ->
    count = 0
    for bot in bots
      # Find the closest point in the box to the bot's center
      closestX = Math.max(boxX, Math.min(bot.x, boxX + boxSize - 1))
      closestY = Math.max(boxY, Math.min(bot.y, boxY + boxSize - 1))
      closestZ = Math.max(boxZ, Math.min(bot.z, boxZ + boxSize - 1))

      distToBox = bot.distanceTo({x: closestX, y: closestY, z: closestZ})

      # If the closest point is within the bot's radius, it potentially intersects
      count++ if distToBox <= bot.r
    count


  # Initialize priority queue
  pq = new PriorityQueue()
  initialPotential = calculatePotentialBots(startX, startY, startZ, initialSize, nanobots)
  initialDist = originDist(startX, startY, startZ) # Distance of the corner closest to origin
  initialBox = new SearchBox(startX, startY, startZ, initialSize, initialPotential, initialDist)
  pq.push initialBox

  maxBotsFound = 0
  bestDist = Infinity

  # Search loop
  while not pq.isEmpty()
    currentBox = pq.pop()

    # Pruning: If this box's potential is not better than the best found so far, skip
    # (Also handles the case where potential is equal but distance is worse implicitly via sorting)
    if currentBox.potentialBots < maxBotsFound
      continue

    # If we reached a single point (box size 1)
    if currentBox.size == 1
      x = currentBox.x
      y = currentBox.y
      z = currentBox.z
      currentPoint = {x, y, z}

      # Calculate actual number of bots in range of this point
      actualBots = nanobots.filter((bot) -> bot.distanceTo(currentPoint) <= bot.r).length
      dist = originDist(x, y, z)

      # Update best result if this point is better
      if actualBots > maxBotsFound
        maxBotsFound = actualBots
        bestDist = dist
      else if actualBots == maxBotsFound
        bestDist = Math.min(bestDist, dist)
      continue # Don't subdivide a single point


    # Subdivide the current box into 8 octants
    newSize = currentBox.size / 2
    for dx in [0, 1]
      for dy in [0, 1]
        for dz in [0, 1]
          nx = currentBox.x + dx * newSize
          ny = currentBox.y + dy * newSize
          nz = currentBox.z + dz * newSize

          potential = calculatePotentialBots(nx, ny, nz, newSize, nanobots)

          # Optimization: Only consider sub-boxes that could potentially beat the current best
          if potential >= maxBotsFound
            dist = originDist(nx, ny, nz) # Distance of the corner closest to origin
            newBox = new SearchBox(nx, ny, nz, newSize, potential, dist)
            pq.push newBox

  bestDist


# --- Main Execution ---

main = ->
  inputFilename = 'input.txt'
  unless fs.existsSync(inputFilename)
    console.error("Error: Input file '#{inputFilename}' not found.")
    console.error("Please create the input file with your puzzle data.")
    # Create a dummy file for demonstration if needed
    fs.writeFileSync(inputFilename, """
    pos=<0,0,0>, r=4
    pos=<1,0,0>, r=1
    pos=<4,0,0>, r=3
    pos=<0,2,0>, r=1
    pos=<0,5,0>, r=3
    pos=<0,0,3>, r=1
    pos=<1,1,1>, r=1
    pos=<1,1,2>, r=1
    pos=<1,3,1>, r=1
    """)
    console.error("Created a dummy input.txt file. Please replace it with your actual data.")
    return

  try
    nanobots = parseInput(inputFilename)

    # --- Part 1 ---
    result1 = solvePart1(nanobots)
    console.log("Part 1:", result1)

    # --- Part 2 ---
    result2 = solvePart2(nanobots)
    console.log("Part 2:", result2)

  catch error
    console.error("An error occurred:", error)


# Run the main function only when the script is executed directly
if require?.main == module
  main()
