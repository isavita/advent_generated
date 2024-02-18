
fs = require 'fs'

directions = [ [0, 1], [1, 0], [0, -1], [-1, 0] ]
pos = { x: 0, y: 0, dirIndex: 0 }

instructions = fs.readFileSync('input.txt').toString().split(', ')

for instruction in instructions
  turn = instruction[0]
  blocks = parseInt(instruction.slice(1))

  if turn == 'R'
    pos.dirIndex = (pos.dirIndex + 1) % 4
  else
    pos.dirIndex = (pos.dirIndex - 1 + 4) % 4

  pos.x += directions[pos.dirIndex][0] * blocks
  pos.y += directions[pos.dirIndex][1] * blocks

console.log(Math.abs(pos.x) + Math.abs(pos.y))
