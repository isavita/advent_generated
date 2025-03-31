
import sequtils
import strutils
import parseutils
import algorithm
import sets

# --- Type Definitions ---

type
  Coord = object
    x, y, z: int

  Brick = object
    id: int # Original index for reference
    p1, p2: Coord

# --- Helper Functions ---

proc minCoord(a, b: int): int = min(a, b)
proc maxCoord(a, b: int): int = max(a, b)

proc minZ(b: Brick): int = minCoord(b.p1.z, b.p2.z)
proc maxZ(b: Brick): int = maxCoord(b.p1.z, b.p2.z)
proc xRange(b: Brick): tuple[min, max: int] = (minCoord(b.p1.x, b.p2.x), maxCoord(b.p1.x, b.p2.x))
proc yRange(b: Brick): tuple[min, max: int] = (minCoord(b.p1.y, b.p2.y), maxCoord(b.p1.y, b.p2.y))

# Check if two 1D ranges overlap
proc rangesOverlap(r1, r2: tuple[min, max: int]): bool =
  max(r1.min, r2.min) <= min(r1.max, r2.max)

# Check if the horizontal projections (XY plane) of two bricks overlap
proc overlapsXY(b1, b2: Brick): bool =
  rangesOverlap(b1.xRange, b2.xRange) and rangesOverlap(b1.yRange, b2.yRange)

# Create a copy of a brick moved down by dz
proc moveDown(b: Brick, dz: int): Brick =
  result = b
  result.p1.z -= dz
  result.p2.z -= dz

# --- Parsing ---

proc parseCoord(s: string): Coord =
  let parts = s.split(',')
  if parts.len != 3: raise newException(ValueError, "Invalid coordinate format: " & s)
  try:
    result = Coord(x: parseInt(parts[0]), y: parseInt(parts[1]), z: parseInt(parts[2]))
  except ValueError as e:
    raise newException(ValueError, "Invalid integer in coordinate: " & s & " | Error: " & e.msg)

proc parseBrick(line: string, id: int): Brick =
  let ends = line.split('~')
  if ends.len != 2: raise newException(ValueError, "Invalid brick format: " & line)
  result = Brick(id: id, p1: parseCoord(ends[0]), p2: parseCoord(ends[1]))
  # Ensure p1.z <= p2.z, swap if necessary (simplifies minZ/maxZ slightly, though not strictly needed with helpers)
  if result.p1.z > result.p2.z:
      swap(result.p1, result.p2)

# --- Core Logic ---

# Simulate bricks falling and settling
proc settleBricks(initialBricks: seq[Brick]): seq[Brick] =
  # Sort bricks by their lowest Z coordinate to process bottom-up
  var sortedBricks = initialBricks
  sortedBricks.sort(proc(a, b: Brick): int = cmp(a.minZ, b.minZ))

  result = newSeq[Brick]() # Stores the final settled positions
  var settledMaxZ = newSeq[int]() # Max Z reached for each XY column (optimization - might not be needed)

  for i in 0 ..< sortedBricks.len:
    var currentBrick = sortedBricks[i]
    var currentMinZ = currentBrick.minZ

    # Simulate falling
    while currentMinZ > 1:
      let nextMinZ = currentMinZ - 1
      var collision = false

      # Check collision with already settled bricks
      for settledBrick in result:
        # Check potential overlap only if the settled brick is directly below
        if settledBrick.maxZ == nextMinZ and overlapsXY(currentBrick, settledBrick):
          collision = true
          break

      if collision:
        break # Cannot fall further, current position is final for this brick

      # No collision, move the brick down one step
      currentBrick = moveDown(currentBrick, 1)
      currentMinZ = nextMinZ
      # Note: Ground collision (z=0) is implicitly handled because loop stops at currentMinZ > 1

    result.add(currentBrick)
    # Sort result by max Z after adding each brick to potentially speed up collision checks
    # Although, iterating through all is simpler and likely fast enough for N ~ 1500
    # result.sort(proc(a, b: Brick): int = cmp(a.maxZ, b.maxZ)) # Optional optimization

# Build support maps: which bricks support which, and which are supported by which
proc buildSupportMaps(settledBricks: seq[Brick]): (seq[seq[int]], seq[seq[int]]) =
  let n = settledBricks.len
  var supports = newSeqWith(n, newSeq[int]())    # supports[i] = list of bricks supported by i
  var supportedBy = newSeqWith(n, newSeq[int]()) # supportedBy[j] = list of bricks supporting j

  # Create a map from original ID to the new index in settledBricks for easier lookup
  var idToIndex: seq[int] = newSeq[int](settledBricks.high + 1) # Assuming IDs are 0..N-1
  for i, brick in settledBricks:
    idToIndex[brick.id] = i

  # Sort settled bricks by Z again just to be sure, though settleBricks should maintain order somewhat
  # This allows checking only j > i potentially, but checking all pairs is safer
  var sortedSettled = settledBricks # Keep original index mapping
  sortedSettled.sort(proc(a, b: Brick): int = cmp(a.minZ, b.minZ))

  for i in 0 ..< n:
    let brickI = sortedSettled[i]
    let idxI = idToIndex[brickI.id]
    for j in i + 1 ..< n:
      let brickJ = sortedSettled[j]
      let idxJ = idToIndex[brickJ.id]

      # Check if brickJ rests directly on brickI
      if brickJ.minZ == brickI.maxZ + 1 and overlapsXY(brickI, brickJ):
        supports[idxI].add(idxJ)
        supportedBy[idxJ].add(idxI)

  return (supports, supportedBy)

# --- Part 1 ---
proc countSafeToDisintegrate(supports: seq[seq[int]], supportedBy: seq[seq[int]]): int =
  result = 0
  let n = supports.len
  for i in 0 ..< n:
    var isEssential = false
    # Check if brick 'i' is the *sole* support for any brick above it
    for j in supports[i]: # For each brick 'j' that 'i' supports
      if supportedBy[j].len == 1:
        # Brick 'j' is only supported by 'i'. Removing 'i' would make 'j' fall.
        isEssential = true
        break # No need to check other bricks supported by 'i'
    if not isEssential:
      result += 1

# --- Part 2 ---
proc sumChainReactionFalls(supports: seq[seq[int]], supportedBy: seq[seq[int]]): int =
  result = 0
  let n = supports.len

  for i in 0 ..< n: # Consider disintegrating brick 'i'
    var q = supports[i] # Queue of bricks potentially falling (initially those directly supported by i)
    var falling = initSet[int]() # Set of bricks that have fallen (including the initial one)
    falling.incl(i)
    var head = 0

    while head < q.len:
      let currentBrickIdx = q[head]
      head += 1

      if currentBrickIdx in falling: continue # Already processed

      # Check if this brick will fall
      var willFall = true
      for supporterIdx in supportedBy[currentBrickIdx]:
        if supporterIdx notin falling:
          willFall = false # Found a support that hasn't fallen
          break

      if willFall:
        falling.incl(currentBrickIdx)
        # Add bricks supported by the newly fallen brick to the queue
        for supportedByCurrent in supports[currentBrickIdx]:
          if supportedByCurrent notin falling:
             q.add(supportedByCurrent) # No need to check duplicates here, check is done at queue processing

    # Add the count of *other* fallen bricks to the total sum
    result += falling.len - 1 # Subtract 1 for the initially disintegrated brick 'i'

# --- Main Execution ---
when isMainModule:
  let inputFile = "input.txt"
  var initialBricks: seq[Brick] = @[]
  try:
    var idCounter = 0
    for line in lines(inputFile):
      if line.strip.len > 0:
        initialBricks.add(parseBrick(line.strip, idCounter))
        idCounter += 1
  except IOError as e:
    echo "Error reading file: ", e.msg
    quit(1)
  except ValueError as e:
    echo "Error parsing input: ", e.msg
    quit(1)

  if initialBricks.len == 0:
    echo "Input file is empty or contains no valid bricks."
    quit(0)

  # 1. Settle the bricks
  let settledBricks = settleBricks(initialBricks)

  # 2. Determine support relationships
  let (supports, supportedBy) = buildSupportMaps(settledBricks)

  # 3. Calculate Part 1
  let part1Result = countSafeToDisintegrate(supports, supportedBy)
  echo "Part 1: ", part1Result

  # 4. Calculate Part 2
  let part2Result = sumChainReactionFalls(supports, supportedBy)
  echo "Part 2: ", part2Result
