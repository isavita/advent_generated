
import std/[heapqueue, tables, strutils, sequtils]
import os

# --- Constants ---
const MaxSteps = 3 # Maximum consecutive steps in one direction

# Directions: 0:Right, 1:Down, 2:Left, 3:Up
const dr = [0, 1, 0, -1]
const dc = [1, 0, -1, 0]
const NumDir = 4

# --- Types ---
# State represents (row, col, direction_entered_from, consecutive_steps_in_that_direction)
# Note: `dir` is the direction taken to *enter* this cell (r, c)
type
  State = tuple[r, c, dir, steps: int]
  # Priority Queue Entry: (total_heat_loss, state)
  PQEntry = tuple[cost: int, state: State]

# --- Priority Queue Comparison ---
# We need a min-heap based on cost.
proc `<`(a, b: PQEntry): bool =
  a.cost < b.cost

# --- Dijkstra's Algorithm ---
proc solve(grid: seq[string]): int =
  let R = grid.len
  let C = grid[0].len
  let targetR = R - 1
  let targetC = C - 1

  # `dist` stores the minimum heat loss found so far to reach a given state.
  # Initialize with a large value (effectively infinity).
  var dist = initTable[State, int]()

  # Priority Queue for Dijkstra's
  var pq = initHeapQueue[PQEntry]()

  # --- Initial States ---
  # We start at (0, 0). The first move can be Right or Down.
  # The cost of the starting cell (0,0) is not incurred.
  # The cost is incurred when *entering* a cell.

  # Move Right to (0, 1)
  if C > 1:
    let initialCostR = grid[0][1].int - '0'.int # Cost to enter (0, 1)
    let initialStateR: State = (r: 0, c: 1, dir: 0, steps: 1) # Entered (0,1) from Left (dir=0), 1st step
    dist[initialStateR] = initialCostR
    pq.push((cost: initialCostR, state: initialStateR))

  # Move Down to (1, 0)
  if R > 1:
    let initialCostD = grid[1][0].int - '0'.int # Cost to enter (1, 0)
    let initialStateD: State = (r: 1, c: 0, dir: 1, steps: 1) # Entered (1,0) from Top (dir=1), 1st step
    dist[initialStateD] = initialCostD
    pq.push((cost: initialCostD, state: initialStateD))

  # --- Main Dijkstra Loop ---
  while pq.len > 0:
    let currentEntry = pq.pop()
    let currentCost = currentEntry.cost
    let currentState = currentEntry.state
    let (r, c, dir, steps) = currentState

    # Optimization: If we've already found a better path to this exact state, skip.
    if currentCost > dist.getOrDefault(currentState, int.high):
      continue

    # Goal Check: If we reached the target cell, return the cost.
    # Dijkstra guarantees the first time we extract the target, it's via the shortest path.
    if r == targetR and c == targetC:
      return currentCost

    # --- Explore Neighbors ---
    for nextDir in 0..<NumDir:
      # Constraint: Cannot reverse direction.
      # If current dir is Right (0), cannot go Left (2). If Down (1), cannot go Up (3).
      if nextDir == (dir + 2) mod NumDir:
        continue

      # Calculate next consecutive steps
      let nextSteps = if nextDir == dir: steps + 1 else: 1

      # Constraint: Cannot move more than MaxSteps consecutively in the same direction.
      if nextSteps > MaxSteps:
        continue

      # Calculate next position
      let nextR = r + dr[nextDir]
      let nextC = c + dc[nextDir]

      # Check if the next position is within grid bounds
      if nextR >= 0 and nextR < R and nextC >= 0 and nextC < C:
        let heatLoss = grid[nextR][nextC].int - '0'.int
        let newCost = currentCost + heatLoss
        let nextState: State = (r: nextR, c: nextC, dir: nextDir, steps: nextSteps)

        # Relaxation: If this path to nextState is better than any known path
        if newCost < dist.getOrDefault(nextState, int.high):
          dist[nextState] = newCost
          pq.push((cost: newCost, state: nextState))

  # Should not be reached if a path exists (which is guaranteed by problem statement)
  return -1

# --- Main Entry Point ---
proc main() =
  # Ensure the input file exists
  if not fileExists("input.txt"):
    echo "Error: input.txt not found."
    quit(1)

  # Read the grid from the input file
  let grid = readFile("input.txt").strip.splitLines

  # Check for empty input
  if grid.len == 0 or grid[0].len == 0:
      echo "Error: input.txt is empty or invalid."
      quit(1)

  # Calculate and print the result
  let minHeatLoss = solve(grid)
  echo minHeatLoss

# --- Execute Main ---
when isMainModule:
  main()
