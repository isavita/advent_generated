
fs = require 'fs'

try
  input = fs.readFileSync('input.txt', 'utf8')
  matches = input.match(/mul\((\d{1,3}),(\d{1,3})\)/g)
  totalSum = 0
  if matches
    for match in matches
      [,x,y] = match.match(/mul\((\d{1,3}),(\d{1,3})\)/)
      totalSum += parseInt(x) * parseInt(y)
  console.log totalSum
catch error
  console.error error
