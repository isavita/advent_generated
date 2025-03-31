
import std/[strutils, sequtils, sets, os]

# --- Type Definitions ---
type Coord = tuple[r, c: int]

# --- Global Variables (or passed around) ---
var grid: seq[string]
var height: int
var width: int

# --- Helper Functions ---

proc isValid(pos: Coord): bool =
  ## Checks if a coordinate is within the grid boundaries.
  pos.r >= 0 and pos.r < height and pos.c >= 0 and pos.c < width

proc getConnectedNeighbors(pos: Coord): seq[Coord] =
  ## Returns the coordinates of valid, directly connected pipe neighbors.
  ## Handles 'S' by checking potential connections *back* to S.
  result = @[]
  if not isValid(pos): return

  let pipe = grid[pos.r][pos.c]
  let r = pos.r
  let c = pos.c
  var potentialDeltas: seq[Coord] = @[] # Relative coordinates

  # Define potential connections based on pipe type
  case pipe:
  of '|': potentialDeltas = @[(r: -1, c: 0), (r: 1, c: 0)] # North, South
  of '-': potentialDeltas = @[(r: 0, c: -1), (r: 0, c: 1)] # West, East
  of 'L': potentialDeltas = @[(r: -1, c: 0), (r: 0, c: 1)] # North, East
  of 'J': potentialDeltas = @[(r: -1, c: 0), (r: 0, c: -1)] # North, West
  of '7': potentialDeltas = @[(r: 1, c: 0), (r: 0, c: -1)] # South, West
  of 'F': potentialDeltas = @[(r: 1, c: 0), (r: 0, c: 1)] # South, East
  of 'S':
    # For 'S', check all four neighbors and see if they *connect back* to S
    # Check North
    let north = (r: r - 1, c: c)
    if isValid(north) and grid[north.r][north.c] in {'|', '7', 'F'}: result.add(north)
    # Check South
    let south = (r: r + 1, c: c)
    if isValid(south) and grid[south.r][south.c] in {'|', 'L', 'J'}: result.add(south)
    # Check West
    let west = (r: r, c: c - 1)
    if isValid(west) and grid[west.r][west.c] in {'-', 'L', 'F'}: result.add(west)
    # Check East
    let east = (r: r, c: c + 1)
    if isValid(east) and grid[east.r][east.c] in {'-', 'J', '7'}: result.add(east)
    return # Return early for 'S', neighbors determined differently
  else: # Ground '.'
    return

  # Calculate absolute coordinates for non-'S' pipes
  for delta in potentialDeltas:
    let neighborPos = (r: r + delta.r, c: c + delta.c)
    # Check if the neighbor is within bounds (isValid is implicitly checked by potentialDeltas definition)
    if isValid(neighborPos):
       result.add(neighborPos)


proc determineSPipe(startPos: Coord): char =
  ## Determines the actual pipe character represented by 'S'.
  let neighbors = getConnectedNeighbors(startPos)
  assert neighbors.len == 2 # The start position must connect to exactly two pipes

  var connectsNorth, connectsSouth, connectsEast, connectsWest: bool

  for neighbor in neighbors:
    if neighbor.r < startPos.r: connectsNorth = true
    if neighbor.r > startPos.r: connectsSouth = true
    if neighbor.c < startPos.c: connectsWest = true
    if neighbor.c > startPos.c: connectsEast = true

  if connectsNorth and connectsSouth: return '|'
  if connectsEast and connectsWest: return '-'
  if connectsNorth and connectsEast: return 'L'
  if connectsNorth and connectsWest: return 'J'
  if connectsSouth and connectsWest: return '7'
  if connectsSouth and connectsEast: return 'F'

  raise newException(ValueError, "Could not determine S pipe type based on neighbors")

# --- Main Logic ---

proc solve(): tuple[part1: int, part2: int] =
  let input = readFile("input.txt")
  grid = input.strip.splitLines
  height = grid.len
  width = if height > 0: grid[0].len else: 0

  if height == 0 or width == 0:
    raise newException(ValueError, "Input grid is empty.")

  # Find Start 'S'
  var startPos: Coord = (-1, -1)
  for r in 0 ..< height:
    for c in 0 ..< width:
      if grid[r][c] == 'S':
        startPos = (r, c)
        break
    if startPos.r != -1: break

  if startPos.r == -1:
    raise newException(ValueError, "Start position 'S' not found.")

  # --- Part 1: Find loop and max distance ---
  var loopTiles = initHashSet[Coord]()
  loopTiles.incl(startPos)

  # Start traversal from one of S's neighbors
  var curr = getConnectedNeighbors(startPos)[0]
  var prev = startPos
  var loopLength = 1 # Start counting distance from the first step

  while curr != startPos:
    loopTiles.incl(curr)

    let neighbors = getConnectedNeighbors(curr)
    var nextPos: Coord = (-1, -1)
    # Find the neighbor that isn't where we just came from
    for neighbor in neighbors:
      if neighbor != prev:
        nextPos = neighbor
        break

    if nextPos.r == -1: # Should not happen in a valid loop
      # Double check if 'S' connects properly if we hit this
       if neighbors.len == 1 and neighbors[0] == prev:
          # This can happen if the grid has dead ends, ensure we are on the main loop path.
          # If we started correctly, this shouldn't occur until we complete the loop.
          raise newException(ValueError, "Loop seems broken or path finding failed at " & $curr)
       elif neighbors.contains(startPos) and neighbors.len >= 1 : # Reached S?
           nextPos = startPos # Explicitly target S if it's a neighbor and not prev
           if nextPos == prev: # We came from S, must go elsewhere
               for neighbor in neighbors:
                   if neighbor != prev:
                       nextPos = neighbor
                       break
           if nextPos == prev: # Still stuck? Error
                raise newException(ValueError, "Could not determine next step from " & $curr & " back towards S")

       else:
            raise newException(ValueError, "Unexpected dead end or loop structure at " & $curr)


    prev = curr
    curr = nextPos
    loopLength += 1

  let maxDistance = loopLength div 2

  # --- Part 2: Count enclosed tiles using Scanline Algorithm ---

  # Replace 'S' with its actual pipe type for the scanline logic
  let sPipeType = determineSPipe(startPos)
  grid[startPos.r][startPos.c] = sPipeType # Modify grid in place

  var insideCount = 0
  for r in 0 ..< height:
    var crossings = 0
    for c in 0 ..< width:
      let currentPos = (r, c)
      if currentPos in loopTiles:
        # Count boundary crossings based on pipes with a vertical component going "up"
        # This corresponds to the non-zero winding number rule / ray casting parity check.
        # We count '|', 'L', and 'J' as crossing the horizontal scanline from below to above.
        case grid[r][c]
        of '|', 'L', 'J':
          crossings += 1
        else: # '-', 'F', '7' do not count as upward vertical crossings
          discard
      else: # Tile is not part of the loop
        if crossings mod 2 == 1:
          # If we've crossed an odd number of boundaries, we are inside
          insideCount += 1

  result = (part1: maxDistance, part2: insideCount)

when isMainModule:
  let (part1Result, part2Result) = solve()
  echo "Part 1: ", part1Result
  echo "Part 2: ", part2Result

