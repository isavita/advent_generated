
fs = require 'fs'

readInputFile = (callback) ->
  inputFile = 'input.txt'
  fs.readFile inputFile, 'utf8', (err, data) ->
    if err
      console.error "Error reading input file: #{err}"
      process.exit(1)
    else
      callback data.split('\n').filter (line) -> line.trim() != ''

isValidTriangle = (a, b, c) ->
  a + b > c and a + c > b and b + c > a

main = (lines) ->
  validTriangles = 0
  for line in lines
    sides = line.split(' ').map (side) -> parseInt(side, 10)
    if sides.length isnt 3
      console.error "Invalid input format: #{line}"
      continue
    if isValidTriangle sides[0], sides[1], sides[2]
      validTriangles++
  console.log validTriangles

readInputFile main
