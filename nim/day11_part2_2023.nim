
import std/[strutils, sequtils, tables, math, os]

type
  Coord = tuple[x, y: int]
  Grid = object
    width, height: int
    data: Table[Coord, char]

const Empty = '.'

proc buildGrid(input: seq[string], empty: char): Grid =
  result = Grid(
    width: input[0].len,
    height: input.len,
    data: initTable[Coord, char]()
  )
  
  for y, line in input:
    for x, ch in line:
      if ch != empty:
        result.data[(x, y)] = ch

proc getEmptyRows(grid: Grid): seq[int] =
  for y in 0..<grid.height:
    var isEmpty = true
    for x in 0..<grid.width:
      if (x, y) in grid.data:
        isEmpty = false
        break
    if isEmpty:
      result.add(y)

proc getEmptyCols(grid: Grid): seq[int] =
  for x in 0..<grid.width:
    var isEmpty = true
    for y in 0..<grid.height:
      if (x, y) in grid.data:
        isEmpty = false
        break
    if isEmpty:
      result.add(x)

proc calculateOffsets(emptyIndexes: seq[int], bound: int): seq[int] =
  result = newSeq[int](bound)
  for idx in emptyIndexes:
    for i in idx+1..<result.len:
      result[i].inc

proc expandGrid(grid: Grid, expansionFactor: int): Grid =
  let 
    emptyCols = grid.getEmptyCols()
    emptyRows = grid.getEmptyRows()
    numLinesToAdd = expansionFactor - 1

  result = Grid(
    width: grid.width + emptyCols.len * numLinesToAdd,
    height: grid.height + emptyRows.len * numLinesToAdd,
    data: initTable[Coord, char]()
  )

  let 
    dXs = calculateOffsets(emptyCols, grid.width)
    dYs = calculateOffsets(emptyRows, grid.height)

  for y in 0..<grid.height:
    for x in 0..<grid.width:
      if (x, y) in grid.data:
        let 
          newX = x + dXs[x] * numLinesToAdd
          newY = y + dYs[y] * numLinesToAdd
        result.data[(newX, newY)] = grid.data[(x, y)]

proc calculateLength(c1, c2: Coord): int =
  abs(c2.x - c1.x) + abs(c2.y - c1.y)

proc solve(input: seq[string], expansionFactor: int): int =
  let 
    grid = buildGrid(input, Empty)
    expandedGrid = expandGrid(grid, expansionFactor)
  
  var alreadySeen = initTable[Coord, bool]()
  
  for coord1 in expandedGrid.data.keys:
    for coord2 in alreadySeen.keys:
      result += calculateLength(coord1, coord2)
    alreadySeen[coord1] = true

proc main() =
  let input = readFile("input.txt").strip().splitLines()
  echo solve(input, 1_000_000)

main()
