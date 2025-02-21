
fs = require 'fs'

grid = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map (row) -> row.split('')

rows = grid.length
cols = grid[0].length

start_row = -1
start_col = -1

for r in [0...rows]
  for c in [0...cols]
    if grid[r][c] == 'S'
      start_row = r
      start_col = c
      break
  break if start_row != -1

reachable = {}
reachable["#{start_row},#{start_col}"] = 0

steps = 64

for i in [1..steps]
  new_reachable = {}
  for coord, step_count of reachable
    row = parseInt(coord.split(',')[0])
    col = parseInt(coord.split(',')[1])

    neighbors = [
      [row - 1, col],
      [row + 1, col],
      [row, col - 1],
      [row, col + 1]
    ]

    for neighbor in neighbors
      nr = neighbor[0]
      nc = neighbor[1]

      if nr >= 0 and nr < rows and nc >= 0 and nc < cols and grid[nr][nc] != '#'
        new_reachable["#{nr},#{nc}"] = i

  reachable = new_reachable
  
count = 0
for coord, step_count of reachable
  if step_count == steps or (steps - step_count) % 2 == 0
      count++

console.log count
