fs = require 'fs'

class Position
  constructor: (@x, @y) ->

# Read input file
input = fs.readFileSync('input.txt', 'utf8').split('\n')

# Initialize grid and starting position
grid = {}
startX = Math.floor(input[0].length / 2)
startY = Math.floor(input.length / 2)

for line, y in input
  for char, x in line
    if char is '#'
      grid["#{x},#{y}"] = true

# Directions: up, right, down, left
dx = [0, 1, 0, -1]
dy = [-1, 0, 1, 0]

x = startX
y = startY
dir = 0 # Start facing up
infectedCount = 0

for i in [0...10000]
  pos = new Position(x, y)
  key = "#{pos.x},#{pos.y}"

  if grid[key]
    dir = (dir + 1) % 4 # Turn right
    delete grid[key] # Clean
  else
    dir = (dir - 1 + 4) % 4 # Turn left
    grid[key] = true # Infect
    infectedCount++

  x += dx[dir]
  y += dy[dir]

console.log(infectedCount)