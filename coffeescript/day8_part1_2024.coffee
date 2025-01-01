
fs = require 'fs'
grid = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
h = grid.length
w = grid[0].length
antennas = {}
for y in [0...h]
  for x in [0...w]
    c = grid[y][x]
    if c != '.'
      antennas[c] ?= []
      antennas[c].push [y, x]
antinodes = {}
for _, coords of antennas
  n = coords.length
  for i in [0...n]
    for j in [i + 1...n]
      A = coords[i]
      B = coords[j]
      P1 = [2 * A[0] - B[0], 2 * A[1] - B[1]]
      P2 = [2 * B[0] - A[0], 2 * B[1] - A[1]]
      if P1[0] >= 0 and P1[0] < h and P1[1] >= 0 and P1[1] < w
        antinodes[P1.join(',')] = true
      if P2[0] >= 0 and P2[0] < h and P2[1] >= 0 and P2[1] < w
        antinodes[P2.join(',')] = true
console.log Object.keys(antinodes).length
