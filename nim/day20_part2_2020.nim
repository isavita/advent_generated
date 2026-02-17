
import strutils, sequtils, math, sets, algorithm

type
  OrientedTileData = object
    contents: seq[string]
    topEdge, bottomEdge, leftEdge, rightEdge: string

  Tile = object
    id: int
    orientations: seq[OrientedTileData]

  AssembledCellInfo = object
    originalTileIdx, orientationIdx: int

proc rotateGrid(grid: seq[string]): seq[string] =
  let n = grid.len
  result = newSeqWith(n, newString(n))
  for r in 0..<n:
    for c in 0..<n:
      result[c][n - 1 - r] = grid[r][c]

proc mirrorGrid(grid: seq[string]): seq[string] =
  result = grid
  for row in result.mitems:
    var s = row
    reverse(s)
    row = s

proc getCol(grid: seq[string], first: bool): string =
  result = ""
  for row in grid:
    result.add(if first: row[0] else: row[^1])

proc generateOrientations(initialGrid: seq[string]): seq[OrientedTileData] =
  var current = initialGrid
  for i in 0..<4:
    for g in [current, mirrorGrid(current)]:
      result.add OrientedTileData(
        contents: g,
        topEdge: g[0],
        bottomEdge: g[^1],
        leftEdge: getCol(g, true),
        rightEdge: getCol(g, false)
      )
    current = rotateGrid(current)

proc solve() =
  let inputStr = readFile("input.txt").strip()
  let blocks = inputStr.split("\n\n")
  var tiles: seq[Tile]
  for b in blocks:
    let lines = b.splitLines()
    let id = lines[0][5..^2].parseInt()
    tiles.add Tile(id: id, orientations: generateOrientations(lines[1..^1]))

  let edgeSize = sqrt(tiles.len.float).int
  var assembled = newSeqWith(edgeSize, newSeq[AssembledCellInfo](edgeSize))
  var used = newSeq[bool](tiles.len)

  proc backtrack(row, col: int): bool =
    if row == edgeSize: return true
    let nextRow = if col == edgeSize - 1: row + 1 else: row
    let nextCol = if col == edgeSize - 1: 0 else: col + 1

    for i in 0..<tiles.len:
      if used[i]: continue
      for oIdx in 0..<tiles[i].orientations.len:
        let opt = tiles[i].orientations[oIdx]
        if row != 0:
          let above = assembled[row-1][col]
          if opt.topEdge != tiles[above.originalTileIdx].orientations[above.orientationIdx].bottomEdge: continue
        if col != 0:
          let left = assembled[row][col-1]
          if opt.leftEdge != tiles[left.originalTileIdx].orientations[left.orientationIdx].rightEdge: continue
        
        assembled[row][col] = AssembledCellInfo(originalTileIdx: i, orientationIdx: oIdx)
        used[i] = true
        if backtrack(nextRow, nextCol): return true
        used[i] = false
    return false

  discard backtrack(0, 0)

  let subTileDim = tiles[0].orientations[0].contents.len - 2
  let fullDim = edgeSize * subTileDim
  var image = newSeqWith(fullDim, "")

  for br in 0..<edgeSize:
    for sr in 0..<subTileDim:
      var rowStr = ""
      for bc in 0..<edgeSize:
        let cell = assembled[br][bc]
        rowStr.add tiles[cell.originalTileIdx].orientations[cell.orientationIdx].contents[sr+1][1..subTileDim]
      image[br * subTileDim + sr] = rowStr

  let monsterPattern = [
    (0, 18),
    (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1, 18), (1, 19),
    (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)
  ]
  let mH = 3
  let mW = 20

  var finalImage: seq[string]
  var monsters: seq[(int, int)]

  var currImg = image
  block finding:
    for i in 0..<4:
      for img in [currImg, mirrorGrid(currImg)]:
        var found: seq[(int, int)]
        for r in 0..(img.len - mH):
          for c in 0..(img[0].len - mW):
            var match = true
            for offset in monsterPattern:
              if img[r + offset[0]][c + offset[1]] != '#':
                match = false; break
            if match:
              for offset in monsterPattern:
                found.add((r + offset[0], c + offset[1]))
        if found.len > 0:
          finalImage = img
          monsters = found
          break finding
      currImg = rotateGrid(currImg)

  var grid = finalImage
  for m in monsters:
    grid[m[0]][m[1]] = 'O'

  var count = 0
  for row in grid:
    for c in row:
      if c == '#': inc count
  echo count

solve()
