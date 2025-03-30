
import tables
import strutils
import sequtils
import math
import algorithm
import os

type Grid = array[25, bool]
type Space = Table[int, Grid]

proc parse(filename: string): Grid =
  result.fill(false)
  var row = 0
  for line in lines(filename):
    let strippedLine = line.strip()
    for col, char in strippedLine:
      if char == '#':
        result[row * 5 + col] = true
    inc row

proc infested(space: Space, level: int, cell: int): bool =
  if space.hasKey(level):
    return space[level][cell]
  else:
    return false

proc minMaxLevel(space: Space): (int, int) =
  var minL = high(int)
  var maxL = low(int)
  if space.len == 0:
    return (0, 0)
  for level in space.keys:
    minL = min(minL, level)
    maxL = max(maxL, level)
  return (minL, maxL)

proc gridIsEmpty(grid: Grid): bool =
  for cell in grid:
    if cell: return false
  return true

proc clean(space: var Space) =
  if space.len == 0: return
  let (minL, maxL) = minMaxLevel(space)

  if space.hasKey(minL) and gridIsEmpty(space[minL]):
      space.del(minL)

  # Check maxL again because minL might have been maxL or it might have been deleted
  if space.hasKey(maxL) and minL != maxL and gridIsEmpty(space[maxL]):
       space.del(maxL)


proc next2(space: Space): Space =
  result = initTable[int, Grid]()
  let (minL, maxL) = minMaxLevel(space)

  for level in (minL - 1) .. (maxL + 1):
    var newGrid: Grid
    # newGrid is implicitly initialized to false (default for array[bool])

    for cell in 0 .. 24:
      if cell == 12: continue

      let row = cell div 5
      let col = cell mod 5
      var neighbours = 0

      if row == 0 and infested(space, level - 1, 7): inc neighbours
      if col == 0 and infested(space, level - 1, 11): inc neighbours
      if col == 4 and infested(space, level - 1, 13): inc neighbours
      if row == 4 and infested(space, level - 1, 17): inc neighbours

      if cell == 7:
        for i in 0..4:
          if infested(space, level + 1, i): inc neighbours
      if cell == 11:
        for i in 0..4:
          if infested(space, level + 1, 5 * i): inc neighbours
      if cell == 13:
        for i in 0..4:
          if infested(space, level + 1, 5 * i + 4): inc neighbours
      if cell == 17:
        for i in 0..4:
          if infested(space, level + 1, 20 + i): inc neighbours

      if row > 0 and cell != 17 and infested(space, level, cell - 5): inc neighbours
      if col > 0 and cell != 13 and infested(space, level, cell - 1): inc neighbours
      if col < 4 and cell != 11 and infested(space, level, cell + 1): inc neighbours
      if row < 4 and cell != 7  and infested(space, level, cell + 5): inc neighbours

      let currentInfested = infested(space, level, cell)
      if currentInfested:
        newGrid[cell] = (neighbours == 1)
      else:
        newGrid[cell] = (neighbours == 1 or neighbours == 2)

    result[level] = newGrid

  clean(result)


proc main() =
  let initialGrid = parse("input.txt")
  var space: Space = initTable[int, Grid]()
  space[0] = initialGrid

  for _ in 1..200:
    space = next2(space)

  var totalBugs = 0
  for grid in space.values:
    for cellIsInfested in grid:
      if cellIsInfested:
        inc totalBugs

  echo totalBugs

when isMainModule:
  main()
