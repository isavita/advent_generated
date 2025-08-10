
import std/[strutils, os, sequtils, tables]

const
  n = 128
  listSize = 256
  rounds = 64
  suffix = @[17, 31, 73, 47, 23]

type
  Grid = array[n, array[n, bool]]

proc reverseSection(a: var array[listSize, int], pos, len: int) =
  var i = pos
  var j = pos + len - 1
  while i < j:
    let ii = i mod listSize
    let jj = j mod listSize
    swap(a[ii], a[jj])
    inc i
    dec j

proc knotHash(input: string): array[16, uint8] =
  var lengths: seq[int]
  for c in input: lengths.add(ord(c))
  lengths.add(suffix)
  var list: array[listSize, int]
  for i in 0 ..< listSize: list[i] = i
  var pos = 0
  var skip = 0
  for _ in 0 ..< rounds:
    for len in lengths:
      reverseSection(list, pos, len)
      pos = (pos + len + skip) mod listSize
      inc skip
  var dense: array[16, uint8]
  for i in 0 ..< 16:
    var x: uint8 = 0
    for j in 0 ..< 16:
      x = x xor uint8(list[i*16 + j])
    dense[i] = x
  dense

proc dfs(x, y: int, g: var Grid) =
  if x < 0 or x >= n or y < 0 or y >= n: return
  if not g[x][y]: return
  g[x][y] = false
  dfs(x-1, y, g)
  dfs(x+1, y, g)
  dfs(x, y-1, g)
  dfs(x, y+1, g)

proc main =
  let key = readFile("input.txt").strip()
  var grid: Grid
  var used = 0
  for i in 0 ..< n:
    let rowKey = key & "-" & $i
    let dense = knotHash(rowKey)
    for j in 0 ..< n:
      let byteIdx = j div 8
      let bitIdx = 7 - (j mod 8)
      let bit = (dense[byteIdx] shr bitIdx) and 1
      if bit == 1:
        grid[i][j] = true
        inc used
  var regions = 0
  for i in 0 ..< n:
    for j in 0 ..< n:
      if grid[i][j]:
        inc regions
        dfs(i, j, grid)
  echo regions

main()
