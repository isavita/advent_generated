
fs = require 'fs'

horizontalPosition = 0
depth = 0

inputFile = fs.readFileSync('input.txt', 'utf8')
lines = inputFile.split('\n')

for line in lines
  command = line.split(' ')
  direction = command[0]
  units = parseInt(command[1])

  switch direction
    when 'forward' then horizontalPosition += units
    when 'down' then depth += units
    when 'up' then depth -= units

product = horizontalPosition * depth
console.log product
