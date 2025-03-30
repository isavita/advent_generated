
import heapqueue
import tables
import strutils
import os

const
  INPUT_FILE = "input.txt"
  WALL = '#'
  START = 'S'
  END = 'E'
  FORWARD_COST = 1
  ROTATE_COST = 1000
  INF = high(int) div 2 # Use a large number for infinity, avoiding overflow

type
  Direction = enum North, East, South, West
  Coord = tuple[r, c: int]
  State = tuple[score: int, pos: Coord, dir: Direction] # Store score first for priority queue ordering

# Delta movements for North, East, South, West
const DR = [-1, 0, 1, 0]
const DC = [0, 1, 0, -1]

# Helper function to check if coordinates are within grid bounds
proc isValid(r, c, R, C: int): bool =
  r >= 0 and r < R and c >= 0 and c < C

# Overload the < operator for the State tuple to work with HeapQueue (min-heap)
proc `<`(a, b: State): bool =
  a.score < b.score

# Rotate direction clockwise
proc rotateCW(dir: Direction): Direction =
  Direction((dir.ord + 1) mod 4)

# Rotate direction counter-clockwise
proc rotateCCW(dir: Direction): Direction =
  Direction((dir.ord + 3) mod 4) # Equivalent to (dir.ord - 1 + 4) mod 4

proc solve(): int =
  # Read input grid
  let lines = readFile(INPUT_FILE).strip().splitLines()
  if lines.len == 0:
    echo "Error: Input file is empty or not found."
    quit(1)

  let R = lines.len
  let C = lines[0].len
  var grid: seq[string] = lines

  # Find Start and End positions
  var startPos, endPos: Coord
  var foundStart, foundEnd = false
  for r in 0..<R:
    for c in 0..<C:
      if grid[r][c] == START:
        startPos = (r, c)
        foundStart = true
      elif grid[r][c] == END:
        endPos = (r, c)
        foundEnd = true
  if not foundStart or not foundEnd:
    echo "Error: Start ('S') or End ('E') marker not found in the grid."
    quit(1)

  # Initialize Dijkstra's algorithm structures
  # `dist` stores the minimum score found so far to reach (r, c) facing `dir`
  var dist = initTable[(Coord, Direction), int]()
  var pq = initHeapQueue[State]()

  # Initial state: at Start, facing East, score 0
  let initialDir = East
  let initialState: State = (score: 0, pos: startPos, dir: initialDir)
  dist[(startPos, initialDir)] = 0
  pq.push(initialState)

  # Main Dijkstra loop
  while pq.len > 0:
    let current = pq.pop()
    let (currentScore, currentPos, currentDir) = current

    # If we reached the end, return the score
    if currentPos == endPos:
      return currentScore

    # Optimization: If we found a shorter path to this state already, skip
    if currentScore > dist.getOrDefault((currentPos, currentDir), INF):
      continue

    # --- Try moving forward ---
    let nextR = currentPos.r + DR[currentDir.ord]
    let nextC = currentPos.c + DC[currentDir.ord]
    let nextPos: Coord = (nextR, nextC)
    let forwardScore = currentScore + FORWARD_COST

    if isValid(nextR, nextC, R, C) and grid[nextR][nextC] != WALL:
      if forwardScore < dist.getOrDefault((nextPos, currentDir), INF):
        dist[(nextPos, currentDir)] = forwardScore
        pq.push((score: forwardScore, pos: nextPos, dir: currentDir))

    # --- Try rotating clockwise ---
    let cwDir = rotateCW(currentDir)
    let rotateScore = currentScore + ROTATE_COST

    if rotateScore < dist.getOrDefault((currentPos, cwDir), INF):
      dist[(currentPos, cwDir)] = rotateScore
      pq.push((score: rotateScore, pos: currentPos, dir: cwDir))

    # --- Try rotating counter-clockwise ---
    let ccwDir = rotateCCW(currentDir)
    # Rotate cost is the same

    if rotateScore < dist.getOrDefault((currentPos, ccwDir), INF):
      dist[(currentPos, ccwDir)] = rotateScore
      pq.push((score: rotateScore, pos: currentPos, dir: ccwDir))

  # If the loop finishes without reaching the end (shouldn't happen based on problem description)
  return -1 # Indicate failure or unreachable

# Main entry point
when isMainModule:
  let result = solve()
  if result != -1:
    echo result
  else:
    echo "Error: Could not find a path from Start to End."

