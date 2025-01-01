
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map (line) -> line.split('')

h = input.length
w = input[0].length

findStart = () ->
  for y in [0...h]
    for x in [0...w]
      switch input[y][x]
        when '^' then return [x, y, 0]
        when '>' then return [x, y, 1]
        when 'v' then return [x, y, 2]
        when '<' then return [x, y, 3]

[startX, startY, startDir] = findStart()
input[startY][startX] = '.'

dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]

loops = (grid, sx, sy, sdir) ->
  x = sx
  y = sy
  dir = sdir
  seen = {}
  for step in [0...2000000]
    key = "#{x},#{y},#{dir}"
    if seen[key]
      return true
    seen[key] = true
    dx = dirs[dir][0]
    dy = dirs[dir][1]
    nx = x + dx
    ny = y + dy
    if nx < 0 or nx >= w or ny < 0 or ny >= h
      return false
    if grid[ny][nx] == '#'
      dir = (dir + 1) % 4
      continue
    x = nx
    y = ny
  return false

canLoop = 0
for y in [0...h]
  for x in [0...w]
    if x == startX and y == startY
      continue
    if input[y][x] != '.'
      continue
    input[y][x] = '#'
    if loops(input, startX, startY, startDir)
      canLoop++
    input[y][x] = '.'

console.log canLoop
