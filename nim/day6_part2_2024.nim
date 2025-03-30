
import std/[strutils, sequtils, sets, os]

type State = tuple[x, y, dirIdx: int]
const dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)] # dx, dy: Up, Right, Down, Left

proc loops(grid: seq[seq[char]], startX, startY, startDir: int): bool =
  let h = grid.len
  let w = grid[0].len
  var x = startX
  var y = startY
  var dirIdx = startDir
  var seen: HashSet[State]

  # Loop until state repeats or goes off grid
  while true:
    let state: State = (x, y, dirIdx)
    if state in seen:
      return true # Loop detected
    seen.incl(state)

    let (dx, dy) = dirs[dirIdx]
    let nx = x + dx
    let ny = y + dy

    # Check bounds first
    if nx < 0 or nx >= w or ny < 0 or ny >= h:
      return false # Went off grid

    # Check for obstacle
    if grid[ny][nx] == '#':
      dirIdx = (dirIdx + 1) mod 4 # Turn right
      # No movement, continue simulation loop
    else:
      # Move forward
      x = nx
      y = ny
      # Continue simulation loop

proc findLoopPositions(inputStr: string): int =
  var grid = inputStr.strip.splitLines.mapIt(it.toSeq)
  if grid.len == 0 or grid[0].len == 0:
      return 0

  let h = grid.len
  let w = grid[0].len

  var startX, startY, startDir: int = -1 # Use -1 to indicate not found yet

  # Find starting position and direction, exit loops once found
  block findStart:
    for r in 0..<h:
      for c in 0..<w:
        case grid[r][c]
        of '^': startX = c; startY = r; startDir = 0; break findStart
        of '>': startX = c; startY = r; startDir = 1; break findStart
        of 'v': startX = c; startY = r; startDir = 2; break findStart
        of '<': startX = c; startY = r; startDir = 3; break findStart
        else: discard

  # Assume valid input guarantees a start position is found
  grid[startY][startX] = '.' # Convert start position to empty space

  var canLoopCount = 0
  for r in 0..<h:
    for c in 0..<w:
      # Skip the original start position
      if r == startY and c == startX: continue

      if grid[r][c] == '.':
        grid[r][c] = '#' # Try placing an obstacle
        if loops(grid, startX, startY, startDir):
          canLoopCount += 1
        grid[r][c] = '.' # Backtrack: remove the obstacle

  return canLoopCount

proc main() =
  let input = readFile("input.txt")
  echo findLoopPositions(input)

when isMainModule:
  main()
