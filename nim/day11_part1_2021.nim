
import strutils, sequtils, sets, os

type
  Grid = seq[seq[int]]
  Coord = tuple[x, y: int]

proc readInput(filename: string): Grid =
  result = newSeq[seq[int]]()
  for line in lines(filename):
    if line.len > 0:
      result.add(line.strip.map(proc(c: char): int = ord(c) - ord('0')))

proc flash(grid: var Grid, x, y: int, flashed: var HashSet[Coord]): int =
  let coord = (x: x, y: y)
  if coord in flashed:
    return 0

  flashed.incl(coord)
  result = 1 # Count this flash

  let height = grid.len
  let width = grid[0].len

  const directions = [
    (-1, -1), (-1, 0), (-1, 1),
    ( 0, -1),          ( 0, 1),
    ( 1, -1), ( 1, 0), ( 1, 1)
  ]

  for dir in directions:
    let newX = x + dir[0]
    let newY = y + dir[1]

    if newX >= 0 and newX < width and newY >= 0 and newY < height:
      grid[newY][newX] += 1
      if grid[newY][newX] > 9:
        # Recursive call will handle the 'notin flashed' check
        result += flash(grid, newX, newY, flashed)

proc simulateStep(grid: var Grid): int =
  var stepFlashes = 0
  var flashed: HashSet[Coord] = initHashSet[Coord]()

  let height = grid.len
  let width = grid[0].len

  # 1. Increment energy levels
  for y in 0..<height:
    for x in 0..<width:
      grid[y][x] += 1

  # 2. Check for flashes and propagate
  for y in 0..<height:
    for x in 0..<width:
      if grid[y][x] > 9:
        # flash handles recursion and adding to 'flashed' set
        stepFlashes += flash(grid, x, y, flashed)

  # 3. Reset flashed octopuses to 0
  for coord in flashed:
    grid[coord.y][coord.x] = 0

  return stepFlashes

proc main() =
  var grid = readInput("input.txt")
  var totalFlashes = 0
  for _ in 1..100:
    totalFlashes += simulateStep(grid)
  echo totalFlashes

when isMainModule:
  main()
