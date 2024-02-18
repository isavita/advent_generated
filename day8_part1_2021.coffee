
fs = require 'fs'

count = 0

input = fs.readFileSync('input.txt', 'utf8').split('\n')

for line in input
  parts = line.split(' | ')
  output = parts[1]
  for digit in output.split(' ')
    switch digit.length
      when 2, 4, 3, 7 then count++

console.log count
