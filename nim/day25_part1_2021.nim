
import strutils, sequtils

proc readGrid(filename: string): seq[seq[char]] =
  result = @[]
  for line in readFile(filename).splitLines:
    result.add(line.toSeq)

proc printGrid(grid: seq[seq[char]]) =
  for row in grid:
    echo row.join("")

proc moveEast(grid: var seq[seq[char]]): bool =
  let rows = grid.len
  let cols = grid[0].len
  var moved = false
  var newGrid = newSeqWith(rows, newSeqWith(cols, '.'))

  for r in 0..<rows:
    for c in 0..<cols:
      if grid[r][c] == '>':
        let nextCol = (c + 1) mod cols
        if grid[r][nextCol] == '.':
          newGrid[r][nextCol] = '>'
          moved = true
        else:
          newGrid[r][c] = '>'
      elif grid[r][c] == 'v':
        newGrid[r][c] = 'v'

  grid = newGrid
  return moved

proc moveSouth(grid: var seq[seq[char]]): bool =
  let rows = grid.len
  let cols = grid[0].len
  var moved = false
  var newGrid = newSeqWith(rows, newSeqWith(cols, '.'))

  for r in 0..<rows:
    for c in 0..<cols:
      if grid[r][c] == 'v':
        let nextRow = (r + 1) mod rows
        if grid[nextRow][c] == '.':
          newGrid[nextRow][c] = 'v'
          moved = true
        else:
          newGrid[r][c] = 'v'
      elif grid[r][c] == '>':
        newGrid[r][c] = '>'

  grid = newGrid
  return moved

proc simulate(grid: var seq[seq[char]]): int =
  var steps = 0
  while true:
    steps += 1
    let eastMoved = moveEast(grid)
    let southMoved = moveSouth(grid)
    if not eastMoved and not southMoved:
      break
  return steps

var grid = readGrid("input.txt")
let result = simulate(grid)
echo "The first step on which no sea cucumbers move is: ", result
