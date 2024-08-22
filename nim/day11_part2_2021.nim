import os
import strutils

type
  Grid = array[0..9, array[0..9, int]]

proc readInput(filename: string): Grid =
  var grid: Grid
  let file = open(filename)
  var row = 0
  for line in file.lines:
    for col, char in line:
      grid[row][col] = parseInt($char)
    inc row
  file.close()
  return grid

proc increaseEnergy(grid: var Grid) =
  for row in 0..9:
    for col in 0..9:
      inc grid[row][col]

proc flash(grid: var Grid, row, col: int, flashed: var array[0..9, array[0..9, bool]]) =
  if row < 0 or row > 9 or col < 0 or col > 9 or flashed[row][col]:
    return
  inc grid[row][col]
  if grid[row][col] > 9:
    grid[row][col] = 0
    flashed[row][col] = true
    for dr in -1..1:
      for dc in -1..1:
        if dr != 0 or dc != 0:
          flash(grid, row + dr, col + dc, flashed)

proc simulate(grid: var Grid): int =
  var step = 0
  while true:
    inc step
    increaseEnergy(grid)
    var flashed: array[0..9, array[0..9, bool]]
    for row in 0..9:
      for col in 0..9:
        if grid[row][col] > 9:
          flash(grid, row, col, flashed)
    var allFlashed = true
    for row in 0..9:
      for col in 0..9:
        if not flashed[row][col]:
          allFlashed = false
          break
      if not allFlashed:
        break
    if allFlashed:
      return step

var grid = readInput("input.txt")
echo simulate(grid)