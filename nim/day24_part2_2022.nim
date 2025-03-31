
import std/[strutils, sequtils, sets, deques, math, streams, tables]

# --- Type Definitions ---
type
  Point = tuple[r, c: int]
  Blizzard = tuple[pos: Point, dir: char]
  State = tuple[pos: Point, time: int]

# --- Math Helpers ---
func gcd(a, b: int): int =
  var x = abs(a)
  var y = abs(b)
  while y != 0:
    let temp = y
    y = x mod y
    x = temp
  return x

func lcm(a, b: int): int =
  if a == 0 or b == 0:
    return 0
  else:
    return abs(a * b) div gcd(a, b)

# --- Core Logic ---

# Calculates the positions of all blizzards at a given time `t`.
# Uses precomputed states for efficiency.
func getBlizzardPositions(t: int, period: int, precomputedBlizzards: seq[HashSet[Point]]): HashSet[Point] =
  precomputedBlizzards[t mod period]

# Breadth-First Search to find the shortest path
func findPath(grid: seq[string], H: int, W: int,
              startPos: Point, endPos: Point, startTime: int,
              period: int, precomputedBlizzards: seq[HashSet[Point]]): int =

  var q = initDeque[State]()
  var visited = initHashSet[tuple[p: Point, tMod: int]]()

  q.addLast((startPos, startTime))
  visited.incl((startPos, startTime mod period))

  let dr = [-1, 1, 0, 0, 0] # Up, Down, Left, Right, Wait
  let dc = [0, 0, -1, 1, 0]

  while q.len > 0:
    let (currentPos, currentTime) = q.popFirst()

    if currentPos == endPos:
      return currentTime

    let nextTime = currentTime + 1
    let nextTimeMod = nextTime mod period
    let blizzardsNext = precomputedBlizzards[nextTimeMod]

    for i in 0..<5:
      let nextR = currentPos.r + dr[i]
      let nextC = currentPos.c + dc[i]
      let nextPos = (r: nextR, c: nextC)

      # Check grid boundaries
      if nextR < 0 or nextR >= H or nextC < 0 or nextC >= W:
        continue

      # Check for walls
      if grid[nextR][nextC] == '#':
        continue

      # Check for blizzards (only need to check inside the inner grid)
      var isBlizzard = false
      # Convert to inner grid coordinates (0-based) for blizzard lookup
      # The blizzard positions in precomputedBlizzards are stored using these inner coords.
      let innerR = nextR - 1
      let innerC = nextC - 1
      # Only check blizzards if the potential next position is within the blizzard-active area
      if nextR > 0 and nextR < H - 1 and nextC > 0 and nextC < W - 1:
         if (r: innerR, c: innerC) in blizzardsNext:
            isBlizzard = true


      if isBlizzard:
         continue


      # Check if already visited this state (position + time modulo period)
      let visitedState = (p: nextPos, tMod: nextTimeMod)
      if visitedState in visited:
        continue

      # If valid and not visited, add to queue and visited set
      q.addLast((nextPos, nextTime))
      visited.incl(visitedState)

  return -1 # Should not happen based on problem description

# --- Main Entry Point ---
proc main() =
  let file = open("input.txt", fmRead)
  defer: file.close()

  let grid = file.readAll().strip().splitLines()
  let H = grid.len
  let W = grid[0].len
  let innerH = H - 2
  let innerW = W - 2

  var initialBlizzards: seq[Blizzard] = @[]
  var startPos: Point
  var endPos: Point

  for r in 0..<H:
    for c in 0..<W:
      case grid[r][c]
      of '^', 'v', '<', '>':
        # Store blizzards with *inner* grid coordinates (0-based)
        initialBlizzards.add((pos: (r: r - 1, c: c - 1), dir: grid[r][c]))
      of '.':
        if r == 0:
          startPos = (r: r, c: c)
        elif r == H - 1:
          endPos = (r: r, c: c)
      else: # '#'
        discard

  # Calculate blizzard cycle period
  let period = lcm(innerH, innerW)

  # Precompute blizzard positions for each minute in the cycle
  var precomputedBlizzards = newSeqWith(period, initHashSet[Point]())

  # Initialize time 0
  for b in initialBlizzards:
    precomputedBlizzards[0].incl(b.pos)

  # Simulate and store for t = 1 to period - 1
  for t in 1..<period:
    let prevBlizzards = precomputedBlizzards[t-1]
    var currentBlizzards = initHashSet[Point]()
    # Need to iterate based on the *initial* blizzards and project their positions
    # Alternatively, iterate through the *previous* set and move each one. Let's stick to initial.
    for b_init in initialBlizzards:
        var r = b_init.pos.r
        var c = b_init.pos.c
        case b_init.dir
        of '^':
            r = (r - t mod innerH + innerH) mod innerH
        of 'v':
            r = (r + t) mod innerH
        of '<':
            c = (c - t mod innerW + innerW) mod innerW
        of '>':
            c = (c + t) mod innerW
        else: discard # Should not happen
        precomputedBlizzards[t].incl((r: r, c: c))


  # Part 1: Start -> End
  let time1 = findPath(grid, H, W, startPos, endPos, 0, period, precomputedBlizzards)
  echo "Part 1: ", time1

  # Part 2: End -> Start -> End
  let time2 = findPath(grid, H, W, endPos, startPos, time1, period, precomputedBlizzards)
  let time3 = findPath(grid, H, W, startPos, endPos, time2, period, precomputedBlizzards)
  echo "Part 2: ", time3

# --- Run ---
main()
