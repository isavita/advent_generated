fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').split('')
floor = 0

input.forEach (char) ->
  if char is '(' then floor++
  else floor--

console.log floor