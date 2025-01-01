
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map (line) -> line.split('')

rows = input.length
cols = input[0].length

start = null
end = null

grid = input.map (row, r) ->
  row.map (cell, c) ->
    if cell == 'S'
      start = [r, c]
      'a'.charCodeAt(0) - 'a'.charCodeAt(0)
    else if cell == 'E'
      end = [r, c]
      'z'.charCodeAt(0) - 'a'.charCodeAt(0)
    else
      cell.charCodeAt(0) - 'a'.charCodeAt(0)

queue = [[start, 0]]
visited = new Set()
visited.add(start.join(','))

while queue.length > 0
  [current, steps] = queue.shift()
  [r, c] = current

  if r == end[0] and c == end[1]
    console.log steps
    break

  moves = [[r - 1, c], [r + 1, c], [r, c - 1], [r, c + 1]]

  for [nr, nc] in moves
    if nr >= 0 and nr < rows and nc >= 0 and nc < cols
      if grid[nr][nc] - grid[r][c] <= 1
        nextPos = [nr, nc]
        nextPosKey = nextPos.join(',')
        if not visited.has(nextPosKey)
          queue.push([nextPos, steps + 1])
          visited.add(nextPosKey)
