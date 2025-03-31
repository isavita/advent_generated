
import std/[heapqueue, tables, strutils, sequtils]

const InputFile = "input.txt"

type
  Grid = seq[seq[int]]
  # State: row, col, direction (0:R, 1:D, 2:L, 3:U), consecutive steps in that dir
  State = tuple[r, c, dir, steps: int]
  DistTable = Table[State, int]
  # Priority Queue stores: (current_heat_loss, state)
  PriorityQueue = HeapQueue[(int, State)]

# Deltas for moving: Right, Down, Left, Up
const dr = [0, 1, 0, -1]
const dc = [1, 0, -1, 0]

# Function to solve the pathfinding problem for given step constraints
proc solve(grid: Grid, minSteps, maxSteps: int): int =
  let rows = grid.len
  let cols = grid[0].len
  let target = (r: rows - 1, c: cols - 1)

  var dist: DistTable = initTable[State, int]()
  var pq: PriorityQueue = initHeapQueue[(int, State)]()

  # Initialize starting states (can move Right or Down from (0, 0))
  # We consider the cost *after* the first step into the grid
  # Start by moving right
  if cols > 1:
    let startStateR = (r: 0, c: 1, dir: 0, steps: 1)
    let startCostR = grid[0][1]
    dist[startStateR] = startCostR
    pq.push((startCostR, startStateR))

  # Start by moving down
  if rows > 1:
    let startStateD = (r: 1, c: 0, dir: 1, steps: 1)
    let startCostD = grid[1][0]
    dist[startStateD] = startCostD
    pq.push((startCostD, startStateD))

  while pq.len > 0:
    let (currentLoss, state) = pq.pop()
    let (r, c, dir, steps) = state

    # If we found a shorter path already, skip
    if currentLoss > dist.getOrDefault(state, high(int)):
      continue

    # Check if we reached the target
    # For Part 2, we must have moved at least minSteps to stop here
    if (r, c) == target and steps >= minSteps:
       return currentLoss

    # Explore next possible moves

    # Try turning left/right (if allowed)
    if steps >= minSteps:
      for turn in [-1, 1]: # -1: Left (relative), +1: Right (relative)
        let newDir = (dir + turn + 4) mod 4 # +4 ensures positive result before mod
        let nr = r + dr[newDir]
        let nc = c + dc[newDir]

        # Check bounds
        if nr >= 0 and nr < rows and nc >= 0 and nc < cols:
          let newLoss = currentLoss + grid[nr][nc]
          let newState = (r: nr, c: nc, dir: newDir, steps: 1)

          if newLoss < dist.getOrDefault(newState, high(int)):
            dist[newState] = newLoss
            pq.push((newLoss, newState))

    # Try continuing straight (if allowed)
    if steps < maxSteps:
      let nr = r + dr[dir]
      let nc = c + dc[dir]

      # Check bounds
      if nr >= 0 and nr < rows and nc >= 0 and nc < cols:
        let newLoss = currentLoss + grid[nr][nc]
        let newState = (r: nr, c: nc, dir: dir, steps: steps + 1)

        if newLoss < dist.getOrDefault(newState, high(int)):
            dist[newState] = newLoss
            pq.push((newLoss, newState))

  return -1 # Should not happen if a path exists

# Main entry point
proc main() =
  # Read input grid
  var grid: Grid = @[]
  try:
    for line in lines(InputFile):
      if line.len > 0:
        grid.add(line.map(proc(c: char): int = parseInt($c)))
  except IOError:
    echo "Error: Cannot read input file: ", InputFile
    quit(1)

  if grid.len == 0 or grid[0].len == 0:
     echo "Error: Input grid is empty."
     quit(1)

  # Solve Part 1 (minSteps=1, maxSteps=3)
  # Note: The problem implies minSteps=1 implicitly for part 1.
  #       You *must* turn after 1, 2, or 3 steps.
  #       So effectively, you *can* turn after 1 step.
  let result1 = solve(grid, 1, 3)
  echo "Part 1: ", result1

  # Solve Part 2 (minSteps=4, maxSteps=10)
  let result2 = solve(grid, 4, 10)
  echo "Part 2: ", result2

# Execute main procedure
main()
