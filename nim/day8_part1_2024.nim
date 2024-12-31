
import strutils, sequtils, tables

let grid = readFile("input.txt").splitLines()
let h = grid.len
let w = grid[0].len
var antennas = initTable[char, seq[array[2, int]]]()

for y in 0..<h:
  for x in 0..<w:
    let c = grid[y][x]
    if c != '.':
      antennas.mgetOrPut(c, @[]).add([y, x])

var antinodes = initTable[array[2, int], bool]()
for coords in antennas.values:
  let n = coords.len
  for i in 0..<n:
    for j in i+1..<n:
      let A = coords[i]
      let B = coords[j]
      let P1 = [2*A[0] - B[0], 2*A[1] - B[1]]
      let P2 = [2*B[0] - A[0], 2*B[1] - A[1]]
      if P1[0] >= 0 and P1[0] < h and P1[1] >= 0 and P1[1] < w:
        antinodes[P1] = true
      if P2[0] >= 0 and P2[0] < h and P2[1] >= 0 and P2[1] < w:
        antinodes[P2] = true

echo antinodes.len
