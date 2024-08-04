fs = require 'fs'

data = fs.readFileSync 'input.txt', 'utf8'
target = parseInt data.trim()

grid = {}
grid['0,0'] = 1

x = 0
y = 0
dx = 0
dy = -1

while true
  if x == y || (x < 0 && x == -y) || (x > 0 && x == 1-y)
    [dx, dy] = [-dy, dx]

  x += dx
  y += dy

  value = 0
  for i in [-1..1]
    for j in [-1..1]
      value += grid["#{x + i},#{y + j}"] or 0

  grid["#{x},#{y}"] = value

  if value > target
    console.log value
    break