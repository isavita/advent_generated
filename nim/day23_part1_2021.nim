
import std/[heapqueue, tables, strutils, math]

# --- Constants and Types ---

const
  hallwayLen = 7
  roomDepth = 2
  numRooms = 4
  stateLen = hallwayLen + numRooms * roomDepth # 7 + 4*2 = 15
  targetState = ".......AABBCCDD"
  invalidIdx = -1

# Use a Table for faster lookups than const array/seq with find
let energyCosts = {'A': 1, 'B': 10, 'C': 100, 'D': 1000}.toTable
let destRoomChar = {'A': 0, 'B': 1, 'C': 2, 'D': 3}.toTable # Destination Room index (0-3) for each char
let destCharRoom: array[numRooms, char] = ['A', 'B', 'C', 'D'] # Destination Char for each room index

# X-coordinates based on the diagram (1-based index for hallway)
# Hallway spots where amphipods can stop
let hallwayX: array[hallwayLen, int] = [1, 2, 4, 6, 8, 10, 11]
# X-coordinate of the space *above* each room entrance
let roomEntranceX: array[numRooms, int] = [3, 5, 7, 9]

type
  State = string # Represents the configuration: 7 hallway spots + 4x2 room spots
  Cost = int64 # Use int64 for potentially large energy costs
  DistTable = Table[State, Cost]
  PriorityQueue = HeapQueue[(Cost, State)]

# --- Helper Functions ---

proc isHallway(idx: int): bool {.inline.} =
  idx >= 0 and idx < hallwayLen

proc isRoom(idx: int): bool {.inline.} =
  idx >= hallwayLen and idx < stateLen

# Returns the room index (0-3) for a state index inside a room
proc roomIndex(idx: int): int {.inline.} =
  assert isRoom(idx)
  (idx - hallwayLen) div roomDepth

# Returns the depth level within a room (0=top, 1=bottom)
proc roomDepthLevel(idx: int): int {.inline.} =
  assert isRoom(idx)
  (idx - hallwayLen) mod roomDepth

# Returns the state index of the top spot (depth 0) of a given room
proc roomStartIdx(roomIdx: int): int {.inline.} =
  hallwayLen + roomIdx * roomDepth

# Get the X coordinate corresponding to a state index
proc getXCoord(idx: int): int =
  if isHallway(idx):
    hallwayX[idx]
  else:
    roomEntranceX[roomIndex(idx)]

# Calculate steps between a room spot and a hallway spot
# Assumes direct path via room entrance
proc stepsBetweenRoomHallway(roomIdx: int, hallIdx: int): int =
  let depth = roomDepthLevel(roomIdx) + 1 # Steps to exit/enter room (1 for top, 2 for bottom)
  let hallDist = abs(getXCoord(hallIdx) - getXCoord(roomIdx))
  result = depth + hallDist

# Check if the hallway path between two X coordinates is clear
# Does NOT check the start/end spots themselves, only intermediate hallway spots
proc isHallwayPathClear(state: State, x1, x2: int): bool =
  let xStart = min(x1, x2)
  let xEnd = max(x1, x2)
  for hIdx in 0 ..< hallwayLen:
    let hx = hallwayX[hIdx]
    if hx > xStart and hx < xEnd: # Check intermediate spots
      if state[hIdx] != '.': return false
  result = true

# Check if an amphipod in a room is blocked by others above it
proc isBlockedInRoom(state: State, roomSpotIdx: int): bool =
  let depth = roomDepthLevel(roomSpotIdx)
  let rIdx = roomIndex(roomSpotIdx)
  let rStart = roomStartIdx(rIdx)
  for d in 0 ..< depth:
    if state[rStart + d] != '.': return true
  result = false

# Check if an amphipod is in its final destination room AND settled
# (i.e., doesn't need to move out)
proc isInFinalPosition(state: State, roomSpotIdx: int): bool =
  let amph = state[roomSpotIdx]
  let rIdx = roomIndex(roomSpotIdx)
  let depth = roomDepthLevel(roomSpotIdx)

  # Must be in the correct room type
  if destRoomChar[amph] != rIdx: return false

  # Check spots below it - they must also be the correct type
  let rStart = roomStartIdx(rIdx)
  for d in (depth + 1) ..< roomDepth:
    if state[rStart + d] != amph: return false

  # If in correct room and all below are correct, it's settled
  result = true

# Check if a room is ready to accept its destined amphipod type
# (i.e., it's empty or contains only the correct type)
proc isRoomReady(state: State, roomIdx: int): bool =
  let expectedAmph = destCharRoom[roomIdx]
  let rStart = roomStartIdx(roomIdx)
  for d in 0 ..< roomDepth:
    let currentAmph = state[rStart + d]
    if currentAmph != '.' and currentAmph != expectedAmph:
      return false # Contains wrong type
  result = true

# Find the deepest available spot index in a ready room, or invalidIdx
proc getTargetRoomSpot(state: State, roomIdx: int): int =
  result = invalidIdx # Assume not found
  let rStart = roomStartIdx(roomIdx)
  # Iterate bottom-up
  for d in countdown(roomDepth - 1, 0):
    let spotIdx = rStart + d
    if state[spotIdx] == '.':
      result = spotIdx # Found the deepest empty spot
      break
    elif state[spotIdx] != destCharRoom[roomIdx]:
        # If we hit a wrong amphipod before finding an empty spot, the room isn't truly ready (or logic error)
        # This check should technically be covered by isRoomReady, but adds safety.
        result = invalidIdx
        break


# --- Dijkstra's Algorithm ---

proc solve(initialState: State): Cost =
  var dist: DistTable = initTable[State, Cost]()
  dist[initialState] = 0.Cost

  var pq: PriorityQueue = initHeapQueue[(Cost, State)]()
  pq.push((0.Cost, initialState))

  while pq.len > 0:
    let (currentCost, currentState) = pq.pop()

    # Optimization: If we found a shorter path already, skip
    if currentCost > dist.getOrDefault(currentState, high(Cost)):
      continue

    # Goal check
    if currentState == targetState:
      return currentCost

    # --- Generate Neighbor States (Moves) ---
    for i in 0 ..< stateLen:
      let amph = currentState[i]
      if amph == '.': continue # Skip empty spots

      let energy = energyCosts[amph]
      let amphDestRoomIdx = destRoomChar[amph]

      # --- Case 1: Amphipod is in a Room ---
      if isRoom(i):
        # Check if it needs to move (not blocked, not already settled)
        if isBlockedInRoom(currentState, i) or isInFinalPosition(currentState, i):
          continue

        let currentRoomIdx = roomIndex(i)
        let startX = getXCoord(i)

        # Try moving to each valid hallway spot
        for hIdx in 0 ..< hallwayLen:
          let targetX = hallwayX[hIdx]

          # Check if hallway path is clear (between room entrance and target hallway spot)
          if isHallwayPathClear(currentState, startX, targetX):
            # Calculate cost
            let steps = stepsBetweenRoomHallway(i, hIdx)
            let moveCost = Cost(steps * energy)
            let newTotalCost = currentCost + moveCost

            # Create next state
            var nextState = currentState
            nextState[i] = '.'
            nextState[hIdx] = amph

            # Update distance if shorter path found
            if newTotalCost < dist.getOrDefault(nextState, high(Cost)):
              dist[nextState] = newTotalCost
              pq.push((newTotalCost, nextState))

      # --- Case 2: Amphipod is in the Hallway ---
      elif isHallway(i):
        # Can only move into its destination room
        let targetRoomIdx = amphDestRoomIdx

        # Check if the destination room is ready
        if not isRoomReady(currentState, targetRoomIdx):
            continue

        let startX = getXCoord(i)
        let targetRoomEntranceX = roomEntranceX[targetRoomIdx]

        # Check if hallway path is clear (between current spot and room entrance)
        if not isHallwayPathClear(currentState, startX, targetRoomEntranceX):
            continue

        # Find the specific spot to move into (deepest available)
        let targetSpotIdx = getTargetRoomSpot(currentState, targetRoomIdx)
        if targetSpotIdx == invalidIdx:
            # This shouldn't happen if isRoomReady passed, but safety check.
            # It means the room is full of the correct type, or has blockers (caught by isRoomReady).
            continue

        # Calculate cost
        let steps = stepsBetweenRoomHallway(targetSpotIdx, i) # Use helper, reverses params
        let moveCost = Cost(steps * energy)
        let newTotalCost = currentCost + moveCost

        # Create next state
        var nextState = currentState
        nextState[i] = '.'
        nextState[targetSpotIdx] = amph

        # Update distance if shorter path found
        if newTotalCost < dist.getOrDefault(nextState, high(Cost)):
          dist[nextState] = newTotalCost
          pq.push((newTotalCost, nextState))

  # Should not be reached if a solution exists
  result = -1

# --- Main Entry Point ---
proc main() =
  let inputLines = readFile("input.txt").strip.splitLines

  # Validate input format assumptions
  if inputLines.len < 4:
    echo "Error: Input file 'input.txt' has fewer than 4 lines."
    quit(1)
  if not (inputLines[2].startsWith("###") and inputLines[2].endsWith("###")) or
     not (inputLines[3].startsWith("  #") and inputLines[3].endsWith("#")):
     echo "Error: Input file format doesn't match expected structure."
     quit(1)

  # Extract initial amphipod positions carefully
  var roomChars: array[numRooms * roomDepth, char]
  try:
    # Room positions (Top row, then Bottom row, Left to Right)
    roomChars[0] = inputLines[2][3] # A Top
    roomChars[1] = inputLines[3][3] # A Bottom
    roomChars[2] = inputLines[2][5] # B Top
    roomChars[3] = inputLines[3][5] # B Bottom
    roomChars[4] = inputLines[2][7] # C Top
    roomChars[5] = inputLines[3][7] # C Bottom
    roomChars[6] = inputLines[2][9] # D Top
    roomChars[7] = inputLines[3][9] # D Bottom
  except IndexError:
    echo "Error: Could not parse amphipod positions from input file. Check format."
    quit(1)

  # Construct initial state string
  var initialState: State = ""
  for _ in 0 ..< hallwayLen: initialState.add('.')
  for c in roomChars: initialState.add(c)

  # Sanity check state length
  if initialState.len != stateLen:
     echo "Internal Error: Constructed initial state has wrong length."
     quit(1)

  # Run the solver
  let result = solve(initialState)

  if result == -1:
    echo "No solution found."
  else:
    echo result

# --- Program Execution ---
when isMainModule:
  main()
