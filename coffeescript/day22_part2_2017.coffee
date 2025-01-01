
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

grid = {}
startX = 0
startY = 0

for y, line of input
  for x, char of line
    if char is '#'
      grid["#{x},#{y}"] = 2
  startX = Math.floor(line.length / 2)
  startY = Math.floor(y / 2)

dx = [0, 1, 0, -1]
dy = [-1, 0, 1, 0]

x = startX
y = startY
dir = 0
infectedCount = 0

for i in [0...10000000]
  key = "#{x},#{y}"
  state = grid[key] or 0
  switch state
    when 0
      dir = (dir - 1 + 4) % 4
      grid[key] = 1
    when 1
      grid[key] = 2
      infectedCount++
    when 2
      dir = (dir + 1) % 4
      grid[key] = 3
    when 3
      dir = (dir + 2) % 4
      grid[key] = 0
  x += dx[dir]
  y += dy[dir]

console.log infectedCount
