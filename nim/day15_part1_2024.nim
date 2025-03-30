
import strutils, tables, sequtils

proc pushBoxes(grid: var seq[seq[char]], r, c, dr, dc: int): bool =
  let nr = r + dr
  let nc = c + dc
  let rows = grid.len
  let cols = grid[0].len

  # Check bounds for the space *beyond* the current box
  if nr < 0 or nr >= rows or nc < 0 or nc >= cols:
    return false # Cannot push off the grid

  let targetChar = grid[nr][nc]

  if targetChar == '#':
    return false # Cannot push into a wall

  var canPlaceBox = false
  if targetChar == '.':
    canPlaceBox = true # Space is empty
  elif targetChar == 'O':
    # Try recursive push. If it succeeds, the spot becomes available.
    if pushBoxes(grid, nr, nc, dr, dc):
      # grid[nr][nc] is now '.', so we can place our box
      canPlaceBox = true
    else:
      # Recursive push failed
      return false
  else: # e.g., '@' - should not happen if logic is sound, but treat as blocked
      return false

  if canPlaceBox:
    # The spot grid[nr][nc] is available
    grid[nr][nc] = 'O' # Place the current box
    grid[r][c] = '.'   # Clear the current box's old spot
    return true
  else:
    # Should not be reached if logic above is correct
    return false

proc solve() =
  let fileContent = readFile("input.txt")
  let lines = fileContent.splitLines()

  var grid: seq[seq[char]] = @[]
  var moves: string = ""
  var readingMap = true

  for line in lines:
    if readingMap:
      if '#' in line:
        grid.add(line.toSeq)
      elif line.strip() == "": # Handle potential empty line between map and moves
         continue
      else:
        readingMap = false
        moves &= line # Start collecting moves
    else:
      moves &= line # Collect subsequent move lines

  if grid.len == 0:
      echo "Error: Grid is empty"
      return

  let rows = grid.len
  let cols = grid[0].len # Assume grid is not empty and rectangular

  var robotR, robotC: int = -1
  block findRobot:
    for r in 0 ..< rows:
      for c in 0 ..< cols:
        if grid[r][c] == '@':
          robotR = r
          robotC = c
          break findRobot

  if robotR == -1:
    echo "Error: Robot not found"
    return

  let dirs = {
    '^': (dr: -1, dc: 0),
    'v': (dr: 1, dc: 0),
    '<': (dr: 0, dc: -1),
    '>': (dr: 0, dc: 1)
  }.toTable

  for move in moves:
    if move notin dirs: continue # Skip invalid move chars

    let dir = dirs[move]
    let dr = dir.dr
    let dc = dir.dc

    let nr = robotR + dr
    let nc = robotC + dc

    # Check bounds for robot's next step
    if nr < 0 or nr >= rows or nc < 0 or nc >= cols:
      continue # Move out of bounds

    let targetChar = grid[nr][nc]

    if targetChar == '#':
      continue # Move into wall

    elif targetChar == 'O':
      # Try pushing the box. Need bounds check within pushBoxes or here.
      # pushBoxes implicitly handles chain reactions and wall checks further down.
      if not pushBoxes(grid, nr, nc, dr, dc):
        continue # Push failed (hit wall or another unmovable box)
      # If push succeeded, grid[nr][nc] is now '.'

    # Check if the target spot is now empty (either initially or after a push)
    if grid[nr][nc] == '.':
        grid[robotR][robotC] = '.' # Clear old robot position
        grid[nr][nc] = '@'       # Move robot to new position
        robotR = nr              # Update robot coordinates
        robotC = nc
    # Else: Spot is still blocked (e.g., push failed), so robot doesn't move.


  var totalSum = 0
  for r in 0 ..< rows:
    for c in 0 ..< cols:
      if grid[r][c] == 'O':
        totalSum += r * 100 + c

  echo totalSum

when isMainModule:
  solve()
