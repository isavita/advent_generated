
import std/os
import std/strutils
import std/sets
import std/hashes

proc solve(): int =
  let inputStr = readFile("input.txt")
  let grid = inputStr.strip(trailing = true).splitLines()
  if grid.len == 0:
    return 0
  let h = grid.len
  let w = grid[0].len

  # Directions: Up(0), Right(1), Down(2), Left(3)
  let dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
  var dirIdx = 0 # Start facing up

  var x, y: int = -1 # Use -1 to indicate not found initially

  # Find starting position '^'
  block findStart:
    for r in 0..<h:
      for c in 0..<w:
        if grid[r][c] == '^':
          x = c
          y = r
          # Start direction is implicitly up (dirIdx = 0)
          break findStart

  # If start wasn't found, maybe return error or 0
  if x == -1:
    return 0 

  var visited = initHashSet[(int, int)]()
  visited.incl((x, y))

  var (dx, dy) = dirs[dirIdx]

  while true:
    let nx = x + dx
    let ny = y + dy

    # Check if we're leaving the grid
    if nx < 0 or nx >= w or ny < 0 or ny >= h:
      break # Exit simulation

    # If obstacle found, turn right (clockwise)
    if grid[ny][nx] == '#':
      dirIdx = (dirIdx + 1) mod 4
      (dx, dy) = dirs[dirIdx]
      # Don't move, just change direction and try again next iteration
      continue

    # Move to new valid position
    x = nx
    y = ny
    visited.incl((x, y))
    # Direction remains the same for the next step

  return visited.len

proc main() =
  echo solve()

when isMainModule:
  main()
