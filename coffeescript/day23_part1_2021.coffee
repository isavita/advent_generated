
###
# Day 23: Amphipod Solver in CoffeeScript
# Solves Part 1 of the Advent of Code 2021 Day 23 challenge.
# Reads input from 'input.txt' and prints the minimum energy cost to stdout.
###

fs = require 'fs'

# --- Priority Queue (Min-Heap Simulation with Sorted Array) ---
# A proper Min-Heap library (like 'heap') is recommended for performance
# on very large state spaces, but this is simpler and sufficient for AoC constraints often.
class PriorityQueue
  constructor: -> @items = [] # Stores [priority, cost, state] tuples

  # Adds an element while maintaining sorted order by priority (ascending).
  push: (priority, cost, state) ->
    element = [priority, cost, state]
    # Use binary search to find the correct insertion index efficiently.
    low = 0
    high = @items.length
    while low < high
      mid = (low + high) >>> 1 # Faster integer division by 2
      if @items[mid][0] < priority
        low = mid + 1
      else
        high = mid
    # Insert the element at the determined position.
    @items.splice(low, 0, element)

  # Removes and returns the element with the lowest priority (at the beginning of the array).
  pop: ->
    @items.shift()

  # Checks if the queue is empty.
  isEmpty: -> @items.length == 0

  # Returns the current size of the queue.
  size: -> @items.length

# --- Constants and Game Configuration ---
COSTS = { A: 1, B: 10, C: 100, D: 1000 }        # Energy cost per step for each amphipod type
TARGET_ROOM = { A: 0, B: 1, C: 2, D: 3 }        # Maps amphipod type to its destination room index
TARGET_TYPE_FOR_ROOM = ['A', 'B', 'C', 'D']     # Maps room index to the target amphipod type
ROOM_ENTRANCE_HALLWAY_POS = [2, 4, 6, 8]        # Hallway index directly above each room entrance
HALLWAY_STOPS = [0, 1, 3, 5, 7, 9, 10]          # Hallway indices where amphipods are allowed to stop
HALLWAY_SIZE = 11                               # Total number of spaces in the hallway row (0-10)
ROOM_DEPTH = 2                                  # Number of spots in each side room (for Part 1)

# --- State Representation and Helper Functions ---

# Creates a unique, canonical string representation of the game state.
# Used as a key for memoization (minCosts map) in Dijkstra's algorithm.
# Format: "HALLWAY_STRING|ROOM0_STR|ROOM1_STR|ROOM2_STR|ROOM3_STR" (e.g., "...........|BA|CD|BC|DA")
stateToString = (state) ->
  hallwayStr = state.hallway.map((x) -> x ? '.').join('')
  # Join room contents, using '|' as separator for clarity and robustness.
  roomsStr = state.rooms.map((room) -> room.map((x) -> x ? '.').join('')).join('|')
  "#{hallwayStr}|#{roomsStr}"

# Creates a deep copy of a state object. Essential to avoid modifying
# states already stored in the priority queue or minCosts map.
cloneState = (state) ->
  # Copy the hallway array (contains primitives or null).
  newHallway = [...state.hallway]
  # Deep copy the rooms array (array of arrays of primitives or null).
  newRooms = state.rooms.map (room) -> [...room]
  # Return the new state object.
  { hallway: newHallway, rooms: newRooms }

# Checks if the given state represents the final, solved configuration.
isGoalState = (state) ->
  # Rule 1: The hallway must be completely empty.
  # 'some' returns true if the callback returns true for any element.
  return false if state.hallway.some (spot) -> spot? # Check if any spot is not null/undefined.

  # Rule 2: Each side room must contain only amphipods of the correct type.
  for roomIdx in [0...state.rooms.length] # Loop through rooms 0, 1, 2, 3
    targetType = TARGET_TYPE_FOR_ROOM[roomIdx] # Get the type expected in this room.
    # Check every spot within the current room.
    for depth in [0...ROOM_DEPTH]
      if state.rooms[roomIdx][depth] != targetType
        # If any spot doesn't contain the correct type, it's not the goal state.
        return false
  
  # If all checks pass, the state is the goal state.
  true

# Checks if the hallway path between two indices (idx1, idx2) is clear.
# "Clear" means all hallway spots strictly between idx1 and idx2 are empty (null).
# The endpoints idx1 and idx2 are not checked.
isHallwayPathClear = (hallway, idx1, idx2) ->
  # Determine the start and end indices for iteration.
  [start, end] = if idx1 < idx2 then [idx1, idx2] else [idx2, idx1]
  # Check all spots strictly between start and end.
  # The '...' range operator in CoffeeScript is exclusive of the end value.
  for i in [start + 1 ... end]
    # If any spot in the path is occupied, the path is blocked.
    return false if hallway[i]?
  # If the loop completes without finding obstructions, the path is clear.
  true

# --- Move Generation Logic ---

# Generates all valid single moves possible from the given state.
# Returns an array of [moveCost, nextState] pairs.
generateMoves = (state) ->
  moves = [] # Initialize list to store generated moves.

  # TYPE 1: Moves from a Room into the Hallway
  for roomIdx in [0...state.rooms.length] # Iterate through each room.
    room = state.rooms[roomIdx]

    # Find the depth of the shallowest amphipod that can potentially move out.
    firstOccupiedDepth = -1
    for depth in [0...ROOM_DEPTH]
      if room[depth]?
        firstOccupiedDepth = depth
        break # Only the topmost amphipod in a room can move out.

    # If the room is empty, or the path out is blocked (handled by finding only the first), skip.
    continue if firstOccupiedDepth == -1

    amphipodType = room[firstOccupiedDepth]

    # Determine if this amphipod *needs* to move out based on the rules:
    # Needs to move if:
    #   a) It's in the wrong destination room.
    #   b) It's in the correct room, but is blocking an amphipod of the wrong type deeper inside.
    isInCorrectRoom = (TARGET_ROOM[amphipodType] == roomIdx)
    blockingWrongType = false
    if isInCorrectRoom
      # Check spots deeper in the same room.
      for depthBelow in [firstOccupiedDepth + 1 ... ROOM_DEPTH]
        occupantBelow = room[depthBelow]
        if occupantBelow? and TARGET_ROOM[occupantBelow] != roomIdx
          blockingWrongType = true
          break # Found one blocking, no need to check further.

    # Only consider moving this amphipod if it needs to move.
    needsToMove = (not isInCorrectRoom) or blockingWrongType
    continue unless needsToMove

    # Calculate the number of steps to reach the hallway space just outside the room.
    stepsOutOfRoom = firstOccupiedDepth + 1
    hallwayEntrancePos = ROOM_ENTRANCE_HALLWAY_POS[roomIdx] # Hallway position above the room.

    # Try moving to each valid hallway stopping position.
    for hallwayDestIdx in HALLWAY_STOPS
      # Check if the path along the hallway (from entrance to destination stop) is clear.
      if isHallwayPathClear(state.hallway, hallwayEntrancePos, hallwayDestIdx)
        # Path is clear. Calculate the total steps and cost.
        stepsInHallway = Math.abs(hallwayDestIdx - hallwayEntrancePos)
        totalSteps = stepsOutOfRoom + stepsInHallway
        moveCost = totalSteps * COSTS[amphipodType]

        # Create the resulting state after this move.
        nextState = cloneState(state)
        nextState.hallway[hallwayDestIdx] = amphipodType     # Place amphipod in hallway stop.
        nextState.rooms[roomIdx][firstOccupiedDepth] = null # Vacate the original room spot.

        moves.push([moveCost, nextState]) # Add the valid move to the list.

  # TYPE 2: Moves from the Hallway into a Destination Room
  for hallwayIdx in [0...HALLWAY_SIZE] # Iterate through each hallway spot.
    amphipodType = state.hallway[hallwayIdx]
    # Skip if the hallway spot is empty.
    continue unless amphipodType?

    # Determine the correct destination room for this amphipod type.
    targetRoomIdx = TARGET_ROOM[amphipodType]
    room = state.rooms[targetRoomIdx]
    targetAmphipodType = TARGET_TYPE_FOR_ROOM[targetRoomIdx] # Type expected in this room.

    # Check if the target room is "ready" according to the rules:
    # Rule: Room must either be empty or contain only amphipods of the correct type.
    roomIsReady = true
    for depth in [0...ROOM_DEPTH]
      occupant = room[depth]
      # If a spot is occupied by the wrong type, the room isn't ready.
      if occupant? and occupant != targetAmphipodType
        roomIsReady = false
        break
    continue unless roomIsReady # If room isn't ready, cannot move in.

    # Check if the path along the hallway (from current spot to room entrance) is clear.
    hallwayEntrancePos = ROOM_ENTRANCE_HALLWAY_POS[targetRoomIdx]
    if isHallwayPathClear(state.hallway, hallwayIdx, hallwayEntrancePos)
        # Hallway path is clear. Find the destination spot within the room.
        # Rule: Amphipod must move to the deepest available spot in the room.
        targetDepth = -1
        # Iterate from the deepest spot (bottom) upwards.
        for depth in [ROOM_DEPTH - 1 .. 0] # `..` is inclusive range in CoffeeScript
          if not room[depth]? # Found the first empty spot from the bottom.
            targetDepth = depth
            break

        # If a valid target depth was found (should always be true if roomIsReady and not full).
        if targetDepth != -1
            # Calculate the total steps and cost for this move.
            stepsInHallway = Math.abs(hallwayIdx - hallwayEntrancePos)
            stepsIntoRoom = targetDepth + 1 # Steps from entrance down to target depth.
            totalSteps = stepsInHallway + stepsIntoRoom
            moveCost = totalSteps * COSTS[amphipodType]

            # Create the resulting state after this move.
            nextState = cloneState(state)
            nextState.hallway[hallwayIdx] = null                # Vacate the hallway spot.
            nextState.rooms[targetRoomIdx][targetDepth] = amphipodType # Place amphipod in room.

            moves.push([moveCost, nextState]) # Add the valid move.

  # Return the list of all valid [moveCost, nextState] pairs found.
  moves


# --- Dijkstra's Algorithm Solver ---
# Finds the minimum energy cost path from an initial state to the goal state.
solve = (initialState) ->
  # Initialize the priority queue with the starting state (cost 0).
  # Priority for Dijkstra is simply the cost accumulated so far.
  pq = new PriorityQueue()
  pq.push(0, 0, initialState) # Format: [priority, cost, state]

  # Keep track of the minimum cost found to reach each visited state.
  # Uses stateToString for keys. Initialize with cost 0 for the initial state.
  minCosts = {}
  initialStateStr = stateToString(initialState)
  minCosts[initialStateStr] = 0

  # Main loop of Dijkstra's algorithm. Continues as long as there are states to explore.
  while not pq.isEmpty()
    # Extract the state with the lowest cost (priority) from the queue.
    [_priority, currentCost, currentState] = pq.pop()

    currentStateStr = stateToString(currentState)

    # Optimization: If we've already found a path to this state with a lower cost,
    # skip processing this one. (Handles cycles and redundant explorations).
    # Use '>' because if currentCost == minCosts[currentStateStr], we don't gain anything by reprocessing.
    if currentCost > minCosts[currentStateStr]
      continue

    # Goal Check: If the current state is the target configuration, we've found the shortest path.
    if isGoalState(currentState)
      return currentCost # Return the minimum cost found.

    # Explore Neighbors: Generate all possible next states reachable from the current state.
    possibleMoves = generateMoves(currentState)

    # Process each valid move and the resulting state.
    for [moveCost, nextState] in possibleMoves
      newCost = currentCost + moveCost # Calculate cost to reach the next state via this path.
      nextStateStr = stateToString(nextState) # Get string representation for memoization.

      # Check if this path to 'nextState' is shorter than any path found previously.
      # Use '? Infinity' as a safe default for states not yet encountered (cost is effectively infinite).
      if newCost < (minCosts[nextStateStr] ? Infinity)
        # Found a new shortest path to 'nextState'. Update its minimum cost.
        minCosts[nextStateStr] = newCost
        # Add the 'nextState' to the priority queue with its new cost as the priority.
        pq.push(newCost, newCost, nextState)

  # If the queue becomes empty and the goal state was never reached, no solution exists.
  return Infinity


# --- Input Parsing ---
# Reads the puzzle input from the specified file and parses it into the initial state object.
parseInput = (filename) ->
  try
    # Read the entire file content.
    lines = fs.readFileSync(filename, 'utf8').trim().split('\n')

    # Input Format Expectation (Part 1):
    # Line 0: #############
    # Line 1: #...........#
    # Line 2: ###B#C#B#D###  <-- Room row 1 (depth 0)
    # Line 3:   #A#D#C#A#    <-- Room row 2 (depth 1)
    # Line 4:   #########

    # Use regular expressions to extract the amphipod types from the room lines.
    # `match()` returns an array; slice `[1..4]` gets the captured groups (the letters).
    roomLine1Chars = lines[2].match(/#(\w)#(\w)#(\w)#(\w)#/)[1..4]
    roomLine2Chars = lines[3].match(/#(\w)#(\w)#(\w)#(\w)#/)[1..4]

    # Create the initial state object structure.
    initialState =
      hallway: Array(HALLWAY_SIZE).fill(null) # Hallway starts empty.
      # Initialize rooms: 4 rooms, each with ROOM_DEPTH spots, initially null.
      rooms: Array(4).fill(null).map -> Array(ROOM_DEPTH).fill(null)

    # Populate the rooms array with the parsed amphipod types.
    for i in [0..3] # Loop through room indices 0 to 3.
      initialState.rooms[i][0] = roomLine1Chars[i] # Top spot (depth 0)
      initialState.rooms[i][1] = roomLine2Chars[i] # Bottom spot (depth 1)

    return initialState # Return the fully parsed initial state.

  catch error
    # Handle potential errors during file reading or parsing.
    console.error "[ERROR] Failed to read or parse input file '#{filename}':", error
    process.exit(1) # Exit the script with an error code.

# --- Main Execution Function ---
# Sets up and runs the puzzle solver.
main = ->
  inputFile = 'input.txt' # Default input file name.

  # Optional: Allow specifying input file via command line argument.
  # `process.argv` contains command line arguments: [node, script.js, arg1, ...]
  if process.argv.length > 2
      inputFile = process.argv[2]

  console.log "--- Day 23: Amphipod (Part 1) ---"
  console.log "[INFO] Reading input from: #{inputFile}"

  # Start timing the overall execution.
  console.time("Total Execution Time")
  try
    # Parse the input file to get the starting state.
    initialState = parseInput(inputFile)

    # console.log "[DEBUG] Initial State:", JSON.stringify(initialState) # Uncomment for debugging

    console.log "[INFO] Calculating minimum energy required..."
    # Run the solver (Dijkstra's algorithm).
    result = solve(initialState)

    console.log("---") # Visual separator

    # Print the final result.
    if result == Infinity
      console.log "RESULT: No solution found."
    else
      console.log "RESULT: Least energy required: #{result}"

  catch error
    # Catch any unexpected errors during the solving process.
    console.error "\n[FATAL ERROR] An unexpected error occurred:", error
    console.error error.stack # Provide stack trace for easier debugging.
  finally
    # Stop timing and print the duration.
    console.log("---")
    console.timeEnd("Total Execution Time")

# --- Script Entry Point ---
# Ensures that the main() function is called only when the script is executed directly
# (e.g., `coffee script.coffee` or `node script.js`), not when required as a module.
if require.main == module
  main()
