
fs = require 'fs'

grid = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map (line) -> line.split('')

h = grid.length
w = grid[0].length
dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]
x = 0
y = 0
dirIdx = 0
found = false

for i in [0...h]
  for j in [0...w]
    switch grid[i][j]
      when '^'
        x = j
        y = i
        dirIdx = 0
        found = true
      when '>'
        x = j
        y = i
        dirIdx = 1
        found = true
      when 'v'
        x = j
        y = i
        dirIdx = 2
        found = true
      when '<'
        x = j
        y = i
        dirIdx = 3
        found = true
    break if found
  break if found

visited = {}
visited["#{x},#{y}"] = true
dirX = dirs[dirIdx][0]
dirY = dirs[dirIdx][1]

while true
  nx = x + dirX
  ny = y + dirY
  if nx < 0 or nx >= w or ny < 0 or ny >= h
    break
  if grid[ny][nx] == '#'
    dirIdx = (dirIdx + 1) % 4
    dirX = dirs[dirIdx][0]
    dirY = dirs[dirIdx][1]
    continue
  x = nx
  y = ny
  visited["#{x},#{y}"] = true

console.log Object.keys(visited).length
