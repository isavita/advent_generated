import std/[sequtils, strutils, tables]

type Grid = array[0..4, array[0..4, char]]

proc parseInput(filename: string): Grid =
  var grid: Grid
  let file = readFile(filename)
  let lines = file.splitLines()
  for i, line in lines:
    for j, char in line:
      grid[i][j] = char
  return grid

proc biodiversityRating(grid: Grid): int =
  var rating = 0
  var power = 1
  for row in grid:
    for cell in row:
      if cell == '#':
        rating += power
      power *= 2
  return rating

proc countAdjacentBugs(grid: Grid, x, y: int): int =
  var count = 0
  if x > 0 and grid[x-1][y] == '#': count.inc
  if x < 4 and grid[x+1][y] == '#': count.inc
  if y > 0 and grid[x][y-1] == '#': count.inc
  if y < 4 and grid[x][y+1] == '#': count.inc
  return count

proc updateGrid(grid: Grid): Grid =
  var newGrid: Grid
  for i in 0..4:
    for j in 0..4:
      let adjacentBugs = countAdjacentBugs(grid, i, j)
      if grid[i][j] == '#' and adjacentBugs != 1:
        newGrid[i][j] = '.'
      elif grid[i][j] == '.' and (adjacentBugs == 1 or adjacentBugs == 2):
        newGrid[i][j] = '#'
      else:
        newGrid[i][j] = grid[i][j]
  return newGrid

proc findFirstDuplicateBiodiversityRating(filename: string): int =
  var grid = parseInput(filename)
  var seenLayouts = initTable[int, bool]()
  while true:
    let rating = biodiversityRating(grid)
    if seenLayouts.hasKey(rating):
      return rating
    seenLayouts[rating] = true
    grid = updateGrid(grid)

when isMainModule:
  let filename = "input.txt"
  let rating = findFirstDuplicateBiodiversityRating(filename)
  echo "Biodiversity rating of the first layout that appears twice: ", rating