fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

grid = []
for i in [0...1000]
  grid[i] = []
  for j in [0...1000]
    grid[i][j] = 0

processInstruction = (instruction) ->
  parts = instruction.split(' ')
  if parts[0] == 'toggle'
    action = 2
    start = parts[1].split(',').map(Number)
    end = parts[3].split(',').map(Number)
  else if parts[1] == 'on'
    action = 1
    start = parts[2].split(',').map(Number)
    end = parts[4].split(',').map(Number)
  else
    action = 0
    start = parts[2].split(',').map(Number)
    end = parts[4].split(',').map(Number)
  
  for i in [start[0]..end[0]]
    for j in [start[1]..end[1]]
      if action == 0
        grid[i][j] = Math.max(0, grid[i][j] - 1)
      else if action == 1
        grid[i][j] += 1
      else
        grid[i][j] += 2

for instruction in input
  processInstruction(instruction)

totalBrightness = 0
for i in [0...1000]
  for j in [0...1000]
    totalBrightness += grid[i][j]

console.log totalBrightness
