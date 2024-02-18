fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8')

floor = 0
position = 0

for char in input
  position++
  if char is '('
    floor++
  else if char is ')'
    floor--
    break if floor < 0

console.log position
