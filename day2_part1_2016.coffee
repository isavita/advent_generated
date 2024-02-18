
fs = require 'fs'

instructions = []
file = fs.readFileSync('input.txt', 'utf8').split('\n')
for line in file
  instructions.push line

keypad = [
  [1, 2, 3]
  [4, 5, 6]
  [7, 8, 9]
]
[x, y] = [1, 1] # Start at '5'
code = ''

for instruction in instructions
  for move in instruction
    switch move
      when 'U' then x -= 1 if x > 0
      when 'D' then x += 1 if x < 2
      when 'L' then y -= 1 if y > 0
      when 'R' then y += 1 if y < 2

  code += keypad[x][y].toString()

console.log code
