
import strutils, sequtils, math, algorithm

type Coord = tuple[x, y: int]
type Grid = object
  width, height: int
  data: seq[Coord]
  chars: seq[char]

const Empty = '.'

proc buildGrid(input: seq[string], empty: char): Grid =
  var data: seq[Coord]
  var chars: seq[char]
  let width = input[0].len
  let height = input.len
  for y, line in input:
    for x, char in line:
      if char != empty:
        data.add((x, y))
        chars.add(char)
  Grid(width: width, height: height, data: data, chars: chars)

proc getEmptyRows(grid: Grid): seq[int] =
  var emptyRows: seq[int]
  for y in 0..<grid.height:
    var isEmpty = true
    for x in 0..<grid.width:
      if (x,y) in grid.data:
        isEmpty = false
        break
    if isEmpty:
      emptyRows.add(y)
  return emptyRows

proc getEmptyCols(grid: Grid): seq[int] =
  var emptyCols: seq[int]
  for x in 0..<grid.width:
    var isEmpty = true
    for y in 0..<grid.height:
      if (x,y) in grid.data:
        isEmpty = false
        break
    if isEmpty:
      emptyCols.add(x)
  return emptyCols

proc calculateOffsets(emptyIndexes: seq[int], bound: int): seq[int] =
  var offsets = newSeq[int](bound)
  for idx in emptyIndexes:
    for i in idx+1..<offsets.len:
      offsets[i] += 1
  return offsets

proc expandGrid(grid: Grid, expansionFactor: int): Grid =
  let emptyCols = grid.getEmptyCols()
  let emptyRows = grid.getEmptyRows()
  let numLinesToAdd = expansionFactor - 1

  let newWidth = grid.width + emptyCols.len * numLinesToAdd
  let newHeight = grid.height + emptyRows.len * numLinesToAdd
  var newData: seq[Coord]
  var newChars: seq[char]

  let dXs = calculateOffsets(emptyCols, grid.width)
  let dYs = calculateOffsets(emptyRows, grid.height)

  for i, coord in grid.data:
    let (x,y) = coord
    let newCoord = (x + dXs[x] * numLinesToAdd, y + dYs[y] * numLinesToAdd)
    newData.add(newCoord)
    newChars.add(grid.chars[i])

  Grid(width: newWidth, height: newHeight, data: newData, chars: newChars)

proc calculateLength(c1, c2: Coord): int =
  abs(c2.x - c1.x) + abs(c2.y - c1.y)

proc solve(input: seq[string]): int =
  let grid = buildGrid(input, Empty)
  let expandedGrid = expandGrid(grid, 2)
  var res = 0
  for i, coord1 in expandedGrid.data:
    for j in 0..<i:
      res += calculateLength(coord1, expandedGrid.data[j])
  return res

let input = readFile("input.txt").splitLines()
echo solve(input)
